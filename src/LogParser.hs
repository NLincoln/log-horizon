{-# LANGUAGE OverloadedStrings #-}

module LogParser (parseLog, LogParseException) where

import Data.Either.Combinators (mapLeft, maybeToRight)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (ZonedTime (ZonedTime))
import Data.Time.Format.ISO8601
import GHC.Exts (IsList (toList))
import Log
import Text.Parsec
import Text.Parsec.String (GenParser, Parser)

data LogParseException = InvalidLogFormat ParseError | NoTimestamp | InvalidTimestampFormat | NoMessage deriving (Show)

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser String
parseString = do
  char '"'
  strings <- many character
  char '"'
  return $ concat strings

termParser :: Parser String
termParser = parseString <|> many (noneOf [' ', '='])

eql :: Parser ()
eql = do
  _ <- oneOf ['=']
  return ()

parseKeyValue :: Parser (String, String)
parseKeyValue = do
  key <- termParser
  _ <- eql
  value <- termParser
  return (key, value)

parseLine :: Parser (Map.Map String String)
parseLine = do
  tuples <- sepBy1 parseKeyValue spaces
  return $ Map.fromList tuples

parseLog :: String -> Either LogParseException Log.Log
parseLog input = do
  parsed <- mapLeft InvalidLogFormat $ parse parseLine "" input
  timestamp <- maybeToRight NoTimestamp $ Map.lookup "timestamp" parsed
  timestampParsed <- maybeToRight InvalidTimestampFormat (iso8601ParseM timestamp :: Maybe ZonedTime)
  message <- maybeToRight NoMessage $ Map.lookup "message" parsed

  return Log {message = T.pack message, timestamp = timestampParsed, context = parsed}
