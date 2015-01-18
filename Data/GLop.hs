module Data.GLop
  ( aggregate
  , printMap
  ) where

import           Data.GLop.Parser ( parseLines )
import           Data.GLop.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M


type EmergeMap = M.Map Package [Int]


aggregate :: BL.ByteString -> EmergeMap
aggregate = aggregateLines . calcDiffs . parseLines


printMap :: EmergeMap -> IO ()
printMap m =
  mapM_ (putStrLn . toString) $ M.toList m
 where
  toString (p, ts) =
    BS.unpack (pkgCategory p) ++
    "/" ++
    BS.unpack (pkgName p) ++
    "\n  " ++
    show (average ts)


average :: [Int] -> Int
average ls = sum ls `div` length ls

aggregateLines :: [(Package, Int)] -> EmergeMap
aggregateLines =
  foldr go M.empty
 where
  go (p, diff) =
    M.insertWith combine p [diff]
  combine [x] xs = x:xs
  combine _ _ = fail "captain! we've been hit"


calcDiffs :: [LogLine] -> [(Package, Int)]
calcDiffs []     = []
calcDiffs (x:xs) =
  snd $ foldr go (x, []) xs
 where
  go line (lst, ls')
    | logPackage line == logPackage lst =
      let diff = logTimestamp lst - logTimestamp line
      in (line, (logPackage line, diff):ls')
    | otherwise = (line, ls')


-- vim: set et sts=2 sw=2 tw=80:
