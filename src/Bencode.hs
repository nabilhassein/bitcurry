{-# LANGUAGE OverloadedStrings #-}

module Bencode (Bencode(..), Dict, parseBencode, antiParse,
                getValue, checkSuccess) where

import Prelude hiding (lookup)
import BTError

import Control.Applicative              ((<|>))
import Data.Attoparsec                  (Parser, many', count, anyWord8, string)
import Data.Attoparsec.ByteString.Char8 (decimal, signed)
import Data.ByteString.Lazy.Char8       (unpack) -- instance IsString ByteString
import Data.Map                         (Map, toList, fromList, lookup)
import Data.Monoid                      ((<>))
import GHC.Word                         (Word8)
import qualified Data.ByteString.Lazy as BL


type Dict    = Map BL.ByteString Bencode
data Bencode = BString BL.ByteString
             | BInt Integer
             | BList [Bencode]
             | BDict Dict
             deriving (Show, Eq, Ord)


-- This does not encode the bytes as decimals, as the spec requires.
-- TODO: fix this and write a test
-- note: using fromIntegral :: Int(eger) -> Word8 computes an answer modulo 256
-- this is intended behavior
packInt :: Integral a => a -> BL.ByteString
packInt                     = BL.pack . reverse . bytes
  where bytes :: Integral a => a -> [Word8] -- little endian
        bytes                  0  = []
        bytes                  n  = fromIntegral n : bytes (n `div` 2^8)

antiParse :: Bencode ->    BL.ByteString
antiParse    (BString s) = packInt (BL.length s) <> ":" <> s
antiParse    (BInt i)    = "i" <> packInt i <> "e"
antiParse    (BList l)   = "l" <> foldr (BL.append . antiParse) "e" l
antiParse    (BDict d)   = "d" <> go (BDict d) <> "e"
  where
  go (BDict dict) = case toList dict of
    []        -> ""
    (k, v):xs -> antiParse (BString k) <> antiParse v <> go (BDict $ fromList xs)

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
getValue :: BL.ByteString -> Dict -> Either BTError Bencode
getValue    bs               dict  = case lookup bs dict of
  Nothing -> Left $ NoKey $ unpack bs
  Just v  -> Right v

checkSuccess :: Dict -> Either BTError ()
checkSuccess    dict  = case lookup "failure reason" dict of
  Just (BString reason) -> Left $ FailureReason $ unpack reason
  _                     -> Right ()
