module Data.GLop
  ( aggregate
  , printMap
  ) where

import           Data.GLop.Parser ( parseLines )
import           Data.GLop.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M


type EmergeMap = M.Map Package [Emerge]


aggregate :: BL.ByteString -> EmergeMap
aggregate = aggregateLines . calcDiffs . parseLines


printMap :: EmergeMap -> IO ()
printMap m =
  mapM_ (putStrLn . toString) $ M.toList m
 where
  toString (p, ts) =
    printPackage p ++
    "\n  " ++
    timeString (average ts)


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


aggregateLines :: [(Package, Emerge)] -> EmergeMap
aggregateLines =
  foldr go M.empty
 where
  go (p, diff) =
    M.insertWith combine p [diff]
  combine [x] xs = x:xs
  combine _ _ = fail "captain! we've been hit"


calcDiffs :: [LogLine] -> [(Package, Emerge)]
calcDiffs []     = []
calcDiffs (x:xs) =
  snd $ foldr go (x, []) xs
 where
  go line (lst, ls')
    | logPackage line == logPackage lst =
      let emerge = Emerge (logTimestamp line) (logTimestamp lst)
      in (line, (logPackage line, emerge):ls')
    | otherwise = (line, ls')


-- vim: set et sts=2 sw=2 tw=80:
