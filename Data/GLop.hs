module Data.GLop
  ( aggregate
  , printMap
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.List             ( sortBy )
import           Data.Function         ( on )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           Data.Time.LocalTime   ( utcToLocalTime, utc )

import           Data.GLop.Parser      ( parseLines )
import           Data.GLop.Types


aggregate :: BL.ByteString -> EmergeMap
aggregate = calcDiffs . parseLines


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
  ordered     = sortBy (flip compare `on` logTimestamp)

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
