module Data.GLop.Types where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M


data LogType =
    EmergeStart
  | EmergeFinish
  deriving ( Show, Eq, Ord )


data Emerge = Emerge Int Int


data Package = Package
  { pkgCategory :: BS.ByteString
  , pkgName     :: BS.ByteString
  } deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { logTimestamp :: Int
  , logPackage   :: Package
  , logProgress  :: (Int, Int)
  , logType      :: LogType
  } deriving ( Show, Eq, Ord )


type EmergeMap = M.Map Package [Emerge]


data RSync = RSync BS.ByteString Int Int


data RSyncLine = RSyncLine
  { rsTime    :: Int
  , rsSource  :: BS.ByteString
  , rsType    :: RSyncType
  } deriving ( Show )


data RSyncType =
    RSyncStart
  | RSyncEnd
  deriving ( Show, Eq )


-- vim: set et sts=2 sw=2 tw=80:
