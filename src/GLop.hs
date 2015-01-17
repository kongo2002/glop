module Main
  ( main
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.GLop.Parser   ( parseLines )
import           System.Environment ( getArgs )


getLogFile :: IO FilePath
getLogFile = do
  args <- getArgs
  case args of
    (f:_) -> return f
    _     -> return logfile
 where
  logfile = "/tmp/emerge.log"


main :: IO ()
main = do
  file <- getLogFile
  ls <- parseLines <$> BL.readFile file
  print ls

-- vim: set et sts=2 sw=2 tw=80:
