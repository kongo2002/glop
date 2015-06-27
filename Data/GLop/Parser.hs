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

import           Data.GLop.Types


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
    (et, prog, pkg) <- emergeType
    toeol
    return $ Just $ LogLine ts pkg prog et


emergeType :: Parser (LogType, (Int, Int), Package)
emergeType = start <|> finish


package :: Parser Package
package = do
  cat <- takeWhile1 (/= '/')
  char '/'
  pkg <- packageName
  return $ Package cat pkg


packageName =
  BS.pack <$> many1 (one <|> noDigit)
 where
  one = satisfy (notInClass "- \t")
  noDigit = do
    char '-'
    c <- peekChar'
    if isDigit c
      then mzero
      else return '-'


start :: Parser (LogType, (Int, Int), Package)
start = do
  string ">>> emerge"
  prog <- progress
  pkg <- package
  return (EmergeStart, prog, pkg)


finish :: Parser (LogType, (Int, Int), Package)
finish = do
  string "::: completed emerge"
  prog <- progress
  pkg <- package
  return (EmergeFinish, prog, pkg)


progress :: Parser (Int, Int)
progress = do
  string " ("
  x <- decimal
  string " of "
  y <- decimal
  string ") "
  return (x, y)


toeol :: Parser ()
toeol = skipWhile (not . iseol)


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'

-- vim: set et sts=2 sw=2 tw=80:
