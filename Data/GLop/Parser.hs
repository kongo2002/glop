{-# LANGUAGE OverloadedStrings #-}

module Data.GLop.Parser
  ( parseFile
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


data LogType =
    EmergeStart
  | EmergeFinish
  deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { logTimestamp :: Int
  , logPackage   :: BS.ByteString
  , logType      :: LogType
  } deriving ( Show, Eq, Ord )


parseFile :: BL.ByteString -> [LogLine]
parseFile ls =
  case AL.parse (logline <* eol) ls of
    AL.Fail {}           -> []
    AL.Done ls' (Just l) -> l : parseFile ls'
    AL.Done ls' _        -> parseFile ls'


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


emergeType :: Parser (LogType, BS.ByteString)
emergeType = start <|> finish


start :: Parser (LogType, BS.ByteString)
start = do
  string ">>> emerge"
  progress
  pkg <- takeWhile1 (not . isSpace)
  return (EmergeStart, pkg)


progress :: Parser ()
progress = do
  string " ("
  many1 digit
  string " of "
  many1 digit
  string ") "
  return ()


finish :: Parser (LogType, BS.ByteString)
finish = do
  string "::: completed emerge"
  progress
  pkg <- takeWhile1 (not . isSpace)
  return (EmergeFinish, pkg)


toeol = skipWhile (not . iseol)


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'
