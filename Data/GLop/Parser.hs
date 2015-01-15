module Data.GLop.Parser where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


data LogType =
    EmergeStart
  | EmergeFinish


data LogLine = LogLine
  { logTimestamp :: Int
  , logPackage   :: BS.ByteString
  , logType      :: LogType
  }


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
    pkg <- takeWhile1 (not . iseol)
    eol
    return $ Just $ LogLine ts pkg EmergeStart


toeol = skipWhile (not . iseol)


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'
