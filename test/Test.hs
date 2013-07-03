{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Bencode (Bencode(..), parseBencode, antiParse)
import           Control.Monad (replicateM)
import           Test.QuickCheck
import           Data.Attoparsec.Lazy (Result(..), parse)
import qualified Data.ByteString.Lazy as BL
import qualified Text.Show.ByteString as TSB


main :: IO ()
main = quickCheck prop_bytestring_inverse

-- TODO: I interpret conformance to the bencoding specification to entail
-- not only acceptance of all valid inputs, but also rejection of all invalid
-- inputs. I'm a hard-ass, not a follower of Postel's Principle.

instance Arbitrary BL.ByteString where
  arbitrary = do
    n      <- choose (1, 1000 :: Int)
    nbytes <- replicateM n arbitrary -- nchars :: [GHC.Word.Word8]
    return $ TSB.show n `BL.append` ":" `BL.append` BL.pack nbytes

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
--       get rid of call to "error", which should never occur
prop_bencode_inverse :: Bencode -> Bool
prop_bencode_inverse bencode =
  case parse parseBencode (antiParse bencode) of
    (Done "" bencode') -> bencode == bencode'
    _                  -> error "either parseBencode or antiParse is broken"
