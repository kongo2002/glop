module Data.GLop
  ( aggregate
  , getCurrent
  , getLast
  , printCurrent
  , printLast
  , printMap
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

import           Data.GLop.Parser      ( parseLines )
import           Data.GLop.Types


aggregate :: BL.ByteString -> EmergeMap
aggregate = calcDiffs . parseLines


getLast :: BL.ByteString -> [(Package, Emerge)]
getLast =
  sortBy comp . M.foldlWithKey' go [] . aggregate
 where
  comp = compare `on` (toTime . snd)
  toTime (Emerge _ t) = t

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
    case logType last' of
      EmergeStart -> Just last'
      _           -> Nothing


printLast :: [(Package, Emerge)] -> IO ()
printLast =
  mapM_ print
 where
  print (pkg, emerge) = do
    putStrLn $ printPackage pkg
    printEmerge emerge


printCurrent :: Maybe (LogLine, EmergeMap) -> IO ()
printCurrent Nothing = putStrLn "no current emerge ongoing"
printCurrent (Just (l, m)) = do
  now <- getCurrentTime
  putStrLn $ printPackage pkg
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
    putStrLn $ printPackage p
    mapM_ printEmerge ts
    putStr "  Average: "
    putStrLn $ timeString (average ts)

  byPackages pkg
    | null ps = True
    | otherwise = any (pkgMatches (fst pkg)) ps


pkgMatches :: Package -> Package -> Bool
pkgMatches (Package c p) (Package c' p') =
  (BS.null c' || c' == c) && (BS.null p' || p' == p)


printEmerge :: Emerge -> IO ()
printEmerge (Emerge start end) = do
  putStr "   "
  putStr $ show startTime
  putStrLn $ " (" ++ timeString duration ++ ")"
 where
  startTime = utcToLocalTime utc (posixSecondsToUTCTime (fromIntegral start))
  duration = end - start


printPackage :: Package -> String
printPackage p =
  BS.unpack (pkgCategory p) ++ "/" ++ BS.unpack (pkgName p)


timeString :: Int -> String
timeString x
  | x < 60    = show x ++ " sec"
  | otherwise = show mins ++ " min " ++ show secs ++ " sec"
 where
  mins = x `div` 60
  secs = x `mod` 60


average :: [Emerge] -> Int
average ls = sum' `div` len
 where
  sum' = sum $ map diff ls
  len = length ls
  diff (Emerge s e) = e - s


toMap :: [(Package, LogLine)] -> M.Map Package [LogLine]
toMap =
  foldr go M.empty
 where
  go (p, diff) = M.insertWith ((:) . head) p [diff]


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

  aggregate' :: [LogLine] -> [Emerge]
  aggregate' []     = []
  aggregate' ls@(l:_) =
    snd $ foldr go (l, []) ls
   where
    go l (lst, ls)
      | isEmerge lst l = (l, Emerge (logTimestamp lst) (logTimestamp l) : ls)
      | otherwise      = (l, ls)

    isEmerge last line =
      let lastType = logType last
          lineType = logType line

      in lastType == EmergeStart &&
         lineType == EmergeFinish &&
         logProgress last == logProgress line


-- vim: set et sts=2 sw=2 tw=80:
