{-# LANGUAGE OverloadedStrings #-}

module Metainfo where

import           Bencode
import           Data.Attoparsec
import           Data.URLEncoded (URLEncoded, urlEncode)
import           System.Posix.Env.ByteString (getArgs)
import qualified Data.ByteString as BS
import qualified Data.Map as Map

main :: IO ()
main = do
  [key] <- getArgs
  contents <- BS.readFile "/home/nabil/Desktop/test.torrent"
  let (Done _ hash) = parse parseBencode contents
--  print $ getKey key hash
  print $ info_hash hash

getKey :: BS.ByteString -> Bencode -> Maybe Bencode
getKey key (BDict hash) = Map.lookup (BString key) hash
getKey _   _            = Nothing


-- parameters for GET request to tracker

info_hash :: Bencode -> URLEncoded
info_hash = urlEncode . getKey "info"
