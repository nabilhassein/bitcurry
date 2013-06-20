{-# LANGUAGE OverloadedStrings #-}

module Bencode where

import           Control.Applicative ((<|>))
import           Data.Attoparsec (Parser, many', count, anyWord8, string)
import           Data.Attoparsec.ByteString.Char8 (decimal, signed)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import qualified Data.Map        as Map


-- see https://wiki.theory.org/BitTorrentSpecification#Bencoding
data Bencode = BString BS.ByteString
             | BInt Integer
             | BList [Bencode]
             | BDict (Map.Map Bencode Bencode)
             deriving (Show, Eq, Ord)


-- TODO: write some tests!
bencode :: Bencode -> BS.ByteString
bencode (BString str) = let n = pack . show $ BS.length str
                        in  n `BS.append` ":" `BS.append` str
bencode (BInt n)      = "i" `BS.append` pack (show n) `BS.append` "e"
bencode (BList xs)    = "l" `BS.append` foldr BS.append "e" (map bencode xs)
bencode (BDict dict)  = "d" `BS.append` helper (BDict dict) `BS.append` "e"
  where helper (BDict hash) = let list = Map.toList hash
                              in case list of
                                []        -> ""
                                (k, v):xs -> bencode k `BS.append`
                                             bencode v `BS.append`
                                             helper (BDict $ Map.fromList xs)


parseBencode :: Parser Bencode
parseBencode = parseString <|> parseInteger <|> parseList <|> parseDictionary

parseString :: Parser Bencode
parseString = do
  n   <- decimal
  _   <- string ":"
  str <- count n anyWord8
  return $ BString $ BS.pack str

parseInteger :: Parser Bencode
parseInteger = do
  _ <- string "i"
  n <- signed decimal
  _ <- string "e"
  return $ BInt n

parseList :: Parser Bencode
parseList = do
  _  <- string "l"
  xs <- many' parseBencode
  _  <- string "e"
  return $ BList xs

parseDictionary :: Parser Bencode
parseDictionary = do
  _  <- string "d"
  xs <- many' parseHash
  _  <- string "e"
  return $ BDict $ Map.fromList xs

parseHash :: Parser (Bencode, Bencode)
parseHash = do
  key <- parseString
  val <- parseBencode
  return (key, val)
