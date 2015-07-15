module Main
  ( main
  ) where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Version       ( showVersion )
import           System.Console.GetOpt
import           System.Environment ( getArgs )
import           System.Exit        ( exitSuccess, exitFailure )
import           System.IO          ( stdin, hPutStrLn, stderr )

import           Data.GLop
import           Data.GLop.Types

import           Paths_glop         ( version )


data Options = Options
  { oFile    :: Maybe String
  , oInput   :: IO BL.ByteString
  , oCurrent :: Bool
  , oLast    :: Bool
  , oRsync   :: Bool
  , oPackage :: [Package]
  }


defOptions :: Options
defOptions = Options
  { oFile      = Nothing
  , oInput     = BL.readFile "/var/log/emerge.log"
  , oCurrent   = False
  , oLast      = False
  , oRsync     = False
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

  , Option "c" []
    (NoArg
      (\opt -> return opt { oCurrent = True }))
    "show current emerge's progress"

  , Option "l" []
    (NoArg
      (\opt -> return opt { oLast = True }))
    "display last emerged packages"

  , Option "s" []
    (NoArg
      (\opt -> return opt { oRsync = True }))
    "display rsync operations"

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
usage = usageInfo header options
 where
  header = unlines
    [ "Usage: glop [OPTIONS] [PACKAGE...]"
    , ""
    , "You may specify PACKAGE in one of three forms:"
    , "  nginx              search for package named 'nginx'"
    , "  www-servers/       search for packages in 'www-servers'"
    , "  www-servers/nginx  search for exact match 'www-servers/nginx'"
    , ""
    , "Apart from specifying the emerge log file via the -f switch"
    , "you may supply the log contents via STDIN by appending a dash:"
    , "  zcat /tmp/emerge.log.gz | glop -"
    ]


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= pos ps >>= multipleFlags
    -- errors
    (_, _, es) -> err $ concat es
 where
  -- process positional arguments
  pos [] opts = return opts
  pos (x:xs) opts =
    case x of
      "-" -> pos xs opts { oInput = getStdIn, oFile = Nothing }
      _ ->
        let pkgs = oPackage opts
        in pos xs opts { oPackage = pkg x : pkgs }

  multipleFlags o
    | toggled > 1 = err "you may only use one of '-c', '-l', '-s'\n"
    | otherwise   = return o
   where
    flags   = [oCurrent o, oLast o, oRsync o]
    toggled = length $ filter id flags

  err msg = let msg' = "error: " ++ msg ++ "\n" ++ usage
            in  hPutStrLn stderr msg' >> exitFailure

  pkg str =
    case break (== '/') str of
      -- just a category
      (c, "/")  -> Package (BS.pack c) BS.empty
      -- category + package
      (c, _:xs) -> Package (BS.pack c) (BS.pack xs)
      -- just a package name otherwise
      (p, _)    -> Package BS.empty (BS.pack p)


main :: IO ()
main =
  operate =<< parseOpts =<< getArgs
 where
  operate opts
    | oCurrent opts = getCurrent <$> input >>= printCurrent
    | oLast opts    = getLast    <$> input >>= printLast
    | oRsync opts   = getRsync   <$> input >>= printRsyncs
    | otherwise     = aggregate  <$> input >>= printMap (oPackage opts)
   where
    input = oInput opts


-- vim: set et sts=2 sw=2 tw=80:
