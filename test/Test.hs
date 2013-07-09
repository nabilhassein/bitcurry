{-# LANGUAGE OverloadedStrings #-}

module Test where

import Bencode              (Bencode(..), parseBencode, antiParse)
import Control.Monad        (replicateM)
import Data.Attoparsec.Lazy (Result(..), parse)
import Data.Map             (fromList)
import Test.QuickCheck      (Arbitrary, Gen, Property, arbitrary, choose,
                             quickCheck, verboseCheck, (==>))
import qualified Data.ByteString.Lazy as BL
import qualified Text.Show.ByteString as TSB


main :: IO ()
main = quickCheck prop_bencode_inverse

-- TODO: I interpret conformance to the bencoding specification to entail
-- not only acceptance of all valid inputs, but also rejection of all invalid
-- inputs. I'm a hard-ass, not a follower of Postel's Principle.

instance Arbitrary BL.ByteString where
  arbitrary = do
    n      <- choose (0, 1000) :: Gen Int
    nbytes <- replicateM n arbitrary
    return $ TSB.show n `BL.append` ":" `BL.append` BL.pack nbytes


instance Arbitrary Bencode where
  arbitrary = do
    n <- choose (1, 3) :: Gen Int
    case n of
      1 -> arbitrary >>= return . BString
      2 -> arbitrary >>= return . BInt
      3 -> choose (0, 2 :: Int) >>= flip replicateM arbitrary >>= return . BList
      4 -> do
        k <- choose (0, 2) :: Gen Int
        kpairs <- replicateM k arbitrary
        return $ BDict $ fromList kpairs

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
