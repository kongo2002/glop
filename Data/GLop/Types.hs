module Data.GLop.Types where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M


data Range =
    Start
  | End
  deriving ( Show, Eq, Ord )


data Operation = Operation Int Int


data Unmerge = Unmerge Package Operation


data Package = Package
  { pkgCategory :: BS.ByteString
  , pkgName     :: BS.ByteString
  } deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { logTimestamp :: Int
  , logPackage   :: Package
  , logProgress  :: (Int, Int)
  , logRange     :: Range
  } deriving ( Show, Eq, Ord )


type EmergeMap = M.Map Package [Operation]


data RSync = RSync BS.ByteString Int Int


data RSyncLine = RSyncLine
  { rsTime    :: Int
  , rsSource  :: BS.ByteString
  , rsType    :: Range
  } deriving ( Show )


data UnmergeLine = UnmergeLine
  { uTime    :: Int
  , uPackage :: Package
  , uType    :: Range
  } deriving ( Show )


-- vim: set et sts=2 sw=2 tw=80:
