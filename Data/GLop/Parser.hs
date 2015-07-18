{-# LANGUAGE OverloadedStrings #-}

module Data.GLop.Parser
  ( parseLines
  , parseRsync
  , parseUnmerge
  ) where

import           Control.Applicative
import           Control.Monad ( mzero )
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import           Data.GLop.Types


parse' parser ls =
  case AL.parse (parser <* eol) ls of
    AL.Fail {}           -> []
    AL.Done ls' (Just l) -> l : parse' parser ls'
    AL.Done ls' _        -> parse' parser ls'


parseLines :: BL.ByteString -> [LogLine]
parseLines = parse' logline


parseRsync :: BL.ByteString -> [RSyncLine]
parseRsync = parse' rsync


parseUnmerge :: BL.ByteString -> [UnmergeLine]
parseUnmerge = parse' unmerge


rsync :: Parser (Maybe RSyncLine)
rsync = rsync' <|> toeol *> pure Nothing
 where
  rsync' = do
    ts <- timestamp
    (t, src) <- rsyncType
    toeol
    return $ Just $ RSyncLine ts src t


unmerge :: Parser (Maybe UnmergeLine)
unmerge = unmerge' <|> toeol *> pure Nothing
 where
  unmerge' = do
    ts <- timestamp
    (t, pkg) <- unmergeType
    toeol
    return $ Just $ UnmergeLine ts pkg t


unmergeType :: Parser (Range, Package)
unmergeType = uStart <|> uEnd
 where
  uStart = do
    string "=== Unmerging... ("
    pkg <- package
    return (Start, pkg)

  uEnd = do
    string ">>> unmerge success: "
    pkg <- package
    return (End, pkg)


rsyncType :: Parser (Range, BS.ByteString)
rsyncType = rsStart <|> rsEnd
 where
  rsStart = do
    string ">>> Starting rsync with "
    source <- takeWhile1 (not . isSpace)
    return (Start, source)

  rsEnd =
    string "=== Sync completed" >> return (End, BS.empty)


logline :: Parser (Maybe LogLine)
logline = line' <|> toeol *> pure Nothing
 where
  line' = do
    ts <- timestamp
    (et, prog, pkg) <- emergeType
    toeol
    return $ Just $ LogLine ts pkg prog et


emergeType :: Parser (Range, (Int, Int), Package)
emergeType = start <|> finish


timestamp :: Parser Int
timestamp = do
  ts <- decimal
  char ':'
  skipWhile isSpace
  return ts


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


start :: Parser (Range, (Int, Int), Package)
start = do
  string ">>> emerge"
  prog <- progress
  pkg <- package
  return (Start, prog, pkg)


finish :: Parser (Range, (Int, Int), Package)
finish = do
  string "::: completed emerge"
  prog <- progress
  pkg <- package
  return (End, prog, pkg)


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
