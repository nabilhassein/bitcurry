{-# LANGUAGE OverloadedStrings #-}

module TrackerClient where

import           Bencode
import           Data.Attoparsec.Lazy (Result(..), parse)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Data.Maybe (fromJust)
import           Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Base (urlEncode, urlEncodeVars)
--import           System.Posix.Env.ByteString (getArgs)
import           System.Random (StdGen, mkStdGen, randomRs)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map

-- temporary testing
-- TODO: write some real tests
main :: IO ()
main = do
  contents <- BL.readFile "../tests/test.torrent"
  let (Done _ hash) = parse parseBencode contents
      url           = constructURL (mkStdGen 42) hash
  putStrLn $ "making GET request to:\n" ++ url
  response <- simpleHTTP $ getRequest url
  body <- getResponseBody response
  print body

-- helper functions
getValue :: BL.ByteString -> Bencode -> Maybe Bencode
getValue key (BDict hash) = Map.lookup (BString key) hash
getValue _   _            = Nothing -- should this error? maybe change the type?

-- parameters for GET request to tracker
-- https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol
-- TODO: write some tests

info_hash :: Bencode -> (String, String)
info_hash hash =
  let value = showDigest . sha1 . antiParse . fromJust $ getValue "info" hash
  in ("info_hash", urlEncode value)

-- from the specification linked above:
-- "[the peer id] must at least be unique for your local machine,
-- thus should probably incorporate things like process ID
-- and perhaps a timestamp recorded at startup"
-- TODO: change seed generation to incorporate advice of above quote
peer_id :: StdGen -> (String, String)
peer_id seed = ("peer_id", "-HS0001-" ++ twelveRandomDigits)
  where twelveRandomDigits = take 12 (randomRs ('0', '9') seed)

-- TODO: have this try 6882 - 6889 in the case of a port already in use
port :: (String, String)
port = ("port", "6881")

-- TODO: have uploaded, downloaded, left actually report meaningful data
uploaded :: (String, String)
uploaded = ("uploaded", "0")

downloaded :: (String, String)
downloaded = ("downloaded", "0")

left :: (String, String)
left = ("left", "0")

-- TODO: research implications of compact
compact :: (String, String)
compact = ("compact", "1")

-- no_peer_id omitted because compact=1 (see specification)

-- TODO: alter this (so probably change type) based on actual events
event :: (String, String)
event = ("event", "started")

-- TODO: include ip, num_want, key, trackerid (optional; temporarily omitted)


-- construct url to use in GET request to tracker
-- TODO: change seed in peer_id in light of TODO above
constructURL :: StdGen -> Bencode -> String
constructURL seed hash =
  let strip   = drop 1 . dropWhile (/= ':') -- for bencoded strings
      baseURL = strip . unpack . antiParse . fromJust $ getValue "announce" hash
  in  baseURL ++ "?" ++ urlEncodeVars [ info_hash hash
                                      , peer_id seed
                                      , port
                                      , uploaded
                                      , downloaded
                                      , left
                                      , compact
                                      , event]
