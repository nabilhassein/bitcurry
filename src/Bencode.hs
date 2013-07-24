{-# LANGUAGE OverloadedStrings #-}

module Bencode (Bencode(..), Dict, parseBencode, antiParse, getValue) where

import Prelude hiding (lookup)

import Control.Applicative              ((<|>))
import Data.Attoparsec                  (Parser, many', count, anyWord8, string)
import Data.Attoparsec.ByteString.Char8 (decimal, signed)
import Data.ByteString.Lazy.Char8       () -- instance IsString ByteString
import Data.Map                         (Map, toList, fromList, lookup)
import qualified Data.ByteString.Lazy as BL


type Dict    = Map BL.ByteString Bencode
data Bencode = BString BL.ByteString
             | BInt Integer
             | BList [Bencode]
             | BDict Dict
             deriving (Show, Eq, Ord)

-- note: using fromIntegral :: Int -> Word8 computes an answer modulo 256
antiParse :: Bencode ->    BL.ByteString
antiParse    (BString s) = fromIntegral (BL.length s) `BL.cons` ":" `BL.append` s
antiParse    (BInt i)    = "i" `BL.append` ((fromIntegral i) `BL.cons` "e")
antiParse    (BList l)   = "l" `BL.append` foldr (BL.append . antiParse) "e" l
antiParse    (BDict d)   = "d" `BL.append` helper (BDict d) `BL.append` "e"
  where helper (BDict hash) = case toList hash of
          []        -> ""
          (k, v):xs -> antiParse (BString k) `BL.append`
                       antiParse v           `BL.append`
                       helper (BDict $ fromList xs)

parseBencode :: Parser Bencode
parseBencode  = parseString <|> parseInteger <|> parseList <|> parseDictionary

parseString :: Parser Bencode
parseString  = do
  n   <- decimal
  _   <- string ":"
  count n anyWord8 >>= return . BString . BL.pack

 -- TODO: reject leading zeroes, and accept only a leading - (not +)
parseInteger :: Parser Bencode
parseInteger  = do
  _ <- string "i"
  n <- signed decimal
  _ <- string "e"
  return $ BInt n

parseList :: Parser Bencode
parseList  = do
  _  <- string "l"
  xs <- many' parseBencode
  _  <- string "e"
  return $ BList xs

-- TODO: from the spec: Keys must be strings and appear in sorted order
-- (sorted as raw strings, not alphanumerics). The strings should be compared
-- using a binary comparison, not a culture-specific "natural" comparison.
parseDictionary :: Parser Bencode
parseDictionary  = do
  _  <- string "d"
  xs <- many' parseDict
  _  <- string "e"
  return $ BDict $ fromList xs

parseDict :: Parser (BL.ByteString, Bencode)
parseDict  = do
  BString key <- parseString
  val         <- parseBencode
  return (key, val)


-- helper functions
getValue :: BL.ByteString -> Dict -> Maybe Bencode
getValue = lookup
