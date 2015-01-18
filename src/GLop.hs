module Main
  ( main
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.GLop          ( aggregate, printMap )
import           System.Environment ( getArgs )
import           System.IO          ( stdin )


getLogFile :: IO BL.ByteString
getLogFile = do
  args <- getArgs
  case args of
    (f:_) ->
      case f of
        "-" -> BL.hGetContents stdin
        _   -> BL.readFile f
    _     -> BL.readFile logfile
 where
  logfile = "/var/log/emerge.log"


main :: IO ()
main = do
  ls <- aggregate <$> getLogFile
  printMap ls

-- vim: set et sts=2 sw=2 tw=80:
