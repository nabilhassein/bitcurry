{-# LANGUAGE DeriveGeneric #-}

module Bencode where

import           Control.Applicative ((<|>))
import           Data.Attoparsec (Parser, word8, many', count, anyWord8)
import           Data.Attoparsec.ByteString.Char8 (decimal, signed)
import           Data.Char (ord)
import           Data.Serialize (Serialize, encode)
import           Data.Word8 (Word8)
import           GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Map        as Map


data Bencode = BString BS.ByteString
             | BInt Integer
             | BList [Bencode]
             | BDict (Map.Map Bencode Bencode)
             deriving (Show, Eq, Ord, Generic)
instance Serialize Bencode


parseBencode :: Parser Bencode
parseBencode = parseString <|> parseInteger <|> parseList <|> parseDictionary

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

parseString :: Parser Bencode
parseString = do
  n   <- decimal
  _   <- word8 $ charToWord8 ':'
  str <- count n anyWord8
  return $ BString $ encode str

parseInteger :: Parser Bencode
parseInteger = do
  _ <- word8 $ charToWord8 'i'
  n <- signed decimal
  _ <- word8 $ charToWord8 'e'
  return $ BInt n

parseList :: Parser Bencode
parseList = do
  _  <- word8 $ charToWord8 'l'
  xs <- many' parseBencode
  _  <- word8 $ charToWord8 'e'
  return $ BList xs

parseDictionary :: Parser Bencode
parseDictionary = do
  _  <- word8 $ charToWord8 'd'
  xs <- many' $ parseHash
  _  <- word8 $ charToWord8 'e'
  return $ BDict $ Map.fromList xs

parseHash :: Parser (Bencode, Bencode)
parseHash = do
  key <- parseString
  val <- parseBencode
  return (key, val)
