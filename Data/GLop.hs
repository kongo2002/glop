module Data.GLop
  ( aggregate
  , getCurrent
  , getLast
  , getRsync
  , getUnmerge
  , printCurrent
  , printLast
  , printMap
  , printRsync
  , printUnmerge
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.List             ( sortBy )
import           Data.Function         ( on )
import           Data.Time.Clock       ( getCurrentTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime
                                       , utcTimeToPOSIXSeconds )
import           Data.Time.LocalTime   ( utcToLocalTime, utc )

import           Data.GLop.Parser
import           Data.GLop.Types


aggregate :: BL.ByteString -> EmergeMap
aggregate = calcDiffs . parseLines


getRsync :: BL.ByteString -> [RSync]
getRsync = aggregateRsyncs . parseRsync


getUnmerge :: BL.ByteString -> [Unmerge]
getUnmerge = aggregateUnmerge . parseUnmerge


getLast :: BL.ByteString -> [(Package, Operation)]
getLast =
  sortBy comp . M.foldlWithKey' go [] . aggregate
 where
  comp = compare `on` (toTime . snd)
  toTime (Operation _ t _) = t

  go xs p es = xs ++ map (\e -> (p, e)) es


getCurrent :: BL.ByteString -> Maybe (LogLine, EmergeMap)
getCurrent input = do
  started <- lastStarted lines'
  return (started, calcDiffs lines')
 where
  lines' = parseLines input
  last'  = last lines'

  lastStarted [] = Nothing
  lastStarted xs =
    case logRange last' of
      Start -> Just last'
      _     -> Nothing


printLast :: [(Package, Operation)] -> IO ()
printLast =
  mapM_ print
 where
  print (pkg, emerge) = do
    printPackage pkg
    printOperation emerge


printCurrent :: Maybe (LogLine, EmergeMap) -> IO ()
printCurrent Nothing = putStrLn "no current emerge ongoing"
printCurrent (Just (l, m)) = do
  now <- getCurrentTime
  printPackage pkg
  case M.lookup pkg m of
    Just ts -> do
      putStrLn $ "  running: " ++ timeString elap
      if elap >= avg
        then putStrLn "  ETA:     just now"
        else putStrLn $ "  ETA:     " ++ timeString (avg - elap)
     where
      avg  = average ts
      elap = truncate (utcTimeToPOSIXSeconds now) - logTimestamp l
    Nothing -> putStrLn "  ETA: unknown"
 where
  pkg = logPackage l


printMap :: [Package] -> EmergeMap -> IO ()
printMap ps m =
  mapM_ toString $ filter byPackages $ M.toList m
 where
  toString (p, ts) = do
    printPackage p
    mapM_ printOperation ts
    putStr "  Average: "
    putStrLn $ timeString (average ts)

  byPackages pkg
    | null ps   = True
    | otherwise = any (pkgMatches (fst pkg)) ps


printUnmerge :: [Package] -> [Unmerge] -> IO ()
printUnmerge ps us =
  mapM_ toString $ filter byPackages us
 where
  toString (Unmerge p op) = do
    printPackage p
    printOperation op

  byPackages (Unmerge p _)
    | null ps   = True
    | otherwise = any (pkgMatches p) ps


pkgMatches :: Package -> Package -> Bool
pkgMatches (Package c p) (Package c' p') =
  (BS.null c' || c' == c) && (BS.null p' || p' == p)


printOperation :: Operation -> IO ()
printOperation (Operation start end version) = do
  putStr $ "   " ++ BS.unpack version ++ ":\n     "
  printTime start
  putStrLn $ " (" ++ timeString duration ++ ")"
 where
  duration = end - start


printTime :: Int -> IO ()
printTime time =
  putStr $ show time'
 where
  time' = utcToLocalTime utc (posixSecondsToUTCTime (fromIntegral time))


printPackage :: Package -> IO ()
printPackage p =
  putStrLn $ un (pkgCategory p) ++ "/" ++ un (pkgName p)
 where
  un = BS.unpack


timeString :: Int -> String
timeString x
  | x < 60    = show x ++ " sec"
  | otherwise = show mins ++ " min " ++ show secs ++ " sec"
 where
  mins = x `div` 60
  secs = x `mod` 60


printRsync :: [RSync] -> IO ()
printRsync [] = putStrLn "no rsync yet"
printRsync rs =
  let (duration, n, ios) = foldr go (0, 0, []) rs
      average            = duration `div` n
  in  sequence_ ios >> putStrLn ("Average: " ++ timeString average)
 where
  go sync (d, n, ios) =
    (d + dur sync, n+1, printRsync' sync : ios)

  dur (RSync _ f t) = t - f


printRsync' :: RSync -> IO ()
printRsync' (RSync src f t) = do
  printTime f
  putStr "\n  "
  BS.putStr src
  putStrLn $ "\n  " ++ timeString duration
 where
  duration = t - f


average :: [Operation] -> Int
average ls = weights `div` wSums
 where
  go :: Operation -> (Int, Int, Int) -> (Int, Int, Int)
  go (Operation s e _) (acc, weight, wghtSum) =
    let duration = e - s
        weighted = duration * weight
    in  (acc + weighted, succ weight, wghtSum + weight)

  (weights, _, wSums) = foldr go (0, 1, 0) ls


toMap :: [(Package, LogLine)] -> M.Map Package [LogLine]
toMap =
  foldr go M.empty
 where
  go (p, diff) = M.insertWith ((:) . head) p [diff]


aggregateUnmerge :: [UnmergeLine] -> [Unmerge]
aggregateUnmerge []       = []
aggregateUnmerge ls@(l:_) =
  snd $ foldr go (l, []) ls
 where
  go l (lst, ls)
    | isUnmerge lst l =
      let op = Operation (uTime l) (uTime lst) (uVersion lst)
      in  (l, Unmerge (uPackage l) op : ls)
    | otherwise       = (l, ls)

  isUnmerge a b =
    uType a == End &&
    uType b == Start &&
    uPackage a == uPackage b


aggregateRsyncs :: [RSyncLine] -> [RSync]
aggregateRsyncs []       = []
aggregateRsyncs ls@(l:_) =
  snd $ foldr go (l, []) ls
 where
  -- because of 'foldr' we are processing the list
  -- in reverse order
  go l (lst, ls)
    | isSync lst l = (l, RSync (rsSource l) (rsTime l) (rsTime lst) : ls)
    | otherwise    = (l, ls)

  isSync a b = rsType a == End && rsType b == Start


calcDiffs :: [LogLine] -> EmergeMap
calcDiffs xs =
  M.filter (not . null) $ M.map (aggregate' . ordered) grouped
 where
  withKey f x = (f x, x)
  grouped     = toMap $ map (withKey logPackage) xs
  ordered     = sortBy cmpLine

  -- at first we sort by the timestamp of the operation and
  -- after that we look at the progress value(s) meaning:
  --   (1 of 3) is less than (2 of 3)
  --
  -- this is important as it is possible to find multiple
  -- entries with the same timestamp value
  cmpLine a b
    | astamp > bstamp = LT
    | astamp < bstamp = GT
    | otherwise =
      let aprog = fst $ logProgress a
          bprog = fst $ logProgress b
      in  compare bprog aprog
   where
    astamp = logTimestamp a
    bstamp = logTimestamp b

  aggregate' :: [LogLine] -> [Operation]
  aggregate' []     = []
  aggregate' ls@(l:_) =
    snd $ foldr go (l, []) ls
   where
    go l (lst, ls)
      | isEmerge lst l = (l, Operation (logTimestamp lst) (logTimestamp l) (logVersion l) : ls)
      | otherwise      = (l, ls)

    isEmerge last line =
      let lastType = logRange last
          lineType = logRange line

      in lastType == Start &&
         lineType == End &&
         logProgress last == logProgress line


-- vim: set et sts=2 sw=2 tw=80:
