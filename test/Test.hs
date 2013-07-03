{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Bencode (Bencode(..), parseBencode, antiParse)
import           Control.Monad (replicateM)
import           Test.QuickCheck
import           Data.Attoparsec.Lazy (Result(..), parse)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL


main :: IO ()
main = quickCheck prop_bytestring_inverse


-- commented out code is what I want, but it doesn't quite work. see Debug.lhs

-- TODO: can we enforce that arbitrary (in the line defining nchars)
--       only takes ASCII characters?
instance Arbitrary BL.ByteString where
  arbitrary = do
    n      <- choose (1, 1000 :: Int)
    -- nchars <- replicateM n arbitrary -- nchars :: [Char]
    nbytes <- replicateM n arbitrary -- nchars :: [GHC.Word.Word8]
    -- return $ (BL.singleton $ fromIntegral n) `BL.append`
    return $ (pack $ show n) `BL.append`
      ":" `BL.append`
      (BL.pack nbytes)

-- tests to write: actual unit tests for parseBencode and antiParse


-- modulo type details: antiParse . parseBencode is id on BL.ByteString
prop_bytestring_inverse :: BL.ByteString -> Property
prop_bytestring_inverse bs = bencodedInput bs ==>
                             let (Done "" bencode) = parse parseBencode bs
                             in antiParse bencode == bs
  where bencodedInput :: BL.ByteString -> Bool
        bencodedInput bl = case parse parseBencode bl of
          (Done "" _) -> True
          _           -> False


-- modulo type details: parseBencode . antiParse is id on Bencode
-- TODO: return Property instead of Bool? can do True ==>, but that seems dumb
prop_bencode_inverse :: Bencode -> Bool
prop_bencode_inverse bencode =
  case parse parseBencode (antiParse bencode) of
    (Done "" bencode') -> bencode == bencode'
    _                  -> error "antiParse is broken"
