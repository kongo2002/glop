module Data.GLop.Types where

import qualified Data.ByteString.Char8 as BS


data LogType =
    EmergeStart
  | EmergeFinish
  deriving ( Show, Eq, Ord )


data Package = Package
  { pkgCategory :: BS.ByteString
  , pkgName     :: BS.ByteString
  } deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { logTimestamp :: Int
  , logPackage   :: Package
  , logType      :: LogType
  } deriving ( Show, Eq, Ord )


-- vim: set et sts=2 sw=2 tw=80:
