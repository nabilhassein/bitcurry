{-# LANGUAGE OverloadedStrings #-}

module Bencode (Bencode(..), parseBencode, antiParse) where

import           Control.Applicative ((<|>))
import           Data.Attoparsec (Parser, many', count, anyWord8, string)
import           Data.Attoparsec.ByteString.Char8 (decimal, signed)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map

-- see https://wiki.theory.org/BitTorrentSpecification#Bencoding
data Bencode = BString BL.ByteString
             | BInt Integer
             | BList [Bencode]
             | BDict (Map.Map Bencode Bencode)
               -- can we restrict the keys to have type BString, as in the spec?
             deriving (Show, Eq, Ord)

-- TODO: write some tests!
-- TODO: make this the "encode" method of Data.Serialize? (also need Generics)
antiParse :: Bencode -> BL.ByteString
antiParse (BString s) = pack (show $ BL.length s) `BL.append` ":" `BL.append` s
antiParse (BInt i)    = "i" `BL.append` pack (show i) `BL.append` "e"
antiParse (BList l)   = "l" `BL.append` foldr (BL.append . antiParse) "e" l
antiParse (BDict d)   = "d" `BL.append` helper (BDict d) `BL.append` "e"
  where helper (BDict hash) = case Map.toList hash of
                                []        -> ""
                                (k, v):xs -> antiParse k `BL.append`
                                             antiParse v `BL.append`
                                             helper (BDict $ Map.fromList xs)


-- TODO: make something like this the "decode" method of Data.Serialize?
parseBencode :: Parser Bencode
parseBencode = parseString <|> parseInteger <|> parseList <|> parseDictionary

parseString :: Parser Bencode
parseString = do
  n   <- decimal
  _   <- string ":"
  str <- count n anyWord8
  return $ BString $ BL.pack str

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

-- make sure this preserves order of dictionary; from the spec:
-- Keys must be strings and appear in sorted order -- (sorted as raw strings,
-- not alphanumerics). The strings should be compared
-- using a binary comparison, not a culture-specific "natural" comparison.
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
