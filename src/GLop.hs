module Main
  ( main
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Version       ( showVersion )
import           System.Console.GetOpt
import           System.Environment ( getArgs )
import           System.Exit        ( exitSuccess )
import           System.IO          ( stdin )

import           Data.GLop          ( aggregate, printMap )
import           Paths_glop         ( version )

data Options = Options
  { oFile    :: Maybe String
  , oInput   :: IO BL.ByteString
  , oPackage :: [String]
  }


defOptions :: Options
defOptions = Options
  { oFile      = Nothing
  , oInput     = BL.readFile "/var/log/emerge.log"
  , oPackage   = []
  }


name :: String
name = "glop"


versionStr :: String
versionStr = showVersion version


fullName :: String
fullName = name ++ "-" ++ versionStr


getStdIn :: IO BL.ByteString
getStdIn = BL.hGetContents stdin


options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "f" ["file"]
    (ReqArg
      (\arg opt -> return opt { oFile = Just arg, oInput = BL.readFile arg })
      "FILE")
    "emerge log file"

  , Option "V" ["version"]
    (NoArg
      (\_ -> putStrLn fullName >> exitSuccess))
    "print version"

  , Option "h" ["help"]
    (NoArg
      (\_ -> putStr usage >> exitSuccess))
    "show this help"
  ]


usage :: String
usage = usageInfo "Usage: glop [PACKAGE...]\n" options


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= pos ps
    -- errors
    (_, _, es) -> ioError (userError (concat es ++ usage))
 where
  -- process positional arguments
  pos [] opts = return opts
  pos (x:xs) opts =
    case x of
      "-" -> pos xs opts { oInput = getStdIn, oFile = Nothing }
      _ ->
        let pkgs = oPackage opts
        in pos xs opts { oPackage = x:pkgs }


main :: IO ()
main = do
  opts <- parseOpts =<< getArgs
  aggregate <$> oInput opts >>= printMap


-- vim: set et sts=2 sw=2 tw=80:
