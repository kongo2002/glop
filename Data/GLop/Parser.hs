{-# LANGUAGE OverloadedStrings #-}

module Data.GLop.Parser
  ( parseLines
  ) where

import           Control.Applicative
import           Control.Monad ( mzero )
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


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


parseLines :: BL.ByteString -> [LogLine]
parseLines ls =
  case AL.parse (logline <* eol) ls of
    AL.Fail {}           -> []
    AL.Done ls' (Just l) -> l : parseLines ls'
    AL.Done ls' _        -> parseLines ls'


logline :: Parser (Maybe LogLine)
logline = line' <|> toeol *> pure Nothing
 where
  line' = do
    ts <- decimal
    char ':'
    skipWhile isSpace
    (et, pkg) <- emergeType
    toeol
    return $ Just $ LogLine ts pkg et


emergeType :: Parser (LogType, Package)
emergeType = start <|> finish


package :: Parser Package
package = do
  cat <- takeWhile1 (/= '/')
  char '/'
  pkg <- packageName
  return $ Package cat pkg


packageName = do
  BS.pack <$> many1 (one <|> noDigit)
 where
  one = satisfy (notInClass "- \t")
  noDigit = do
    char '-'
    c <- peekChar'
    if c >= '0' && c <= '9'
      then mzero
      else return '-'


start :: Parser (LogType, Package)
start = do
  string ">>> emerge"
  progress
  pkg <- package
  return (EmergeStart, pkg)


finish :: Parser (LogType, Package)
finish = do
  string "::: completed emerge"
  progress
  pkg <- package
  return (EmergeFinish, pkg)


progress :: Parser ()
progress = do
  string " ("
  many1 digit
  string " of "
  many1 digit
  string ") "
  return ()


toeol :: Parser ()
toeol = skipWhile (not . iseol)


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'

-- vim: set et sts=2 sw=2 tw=80:
