{-# LANGUAGE OverloadedStrings #-}

module TrackerClient where

import           Bencode (Bencode(..), antiParse, parseBencode)
import           Data.Attoparsec.Lazy (Result(..), parse)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Data.Maybe (fromJust)
import           Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Base (urlEncode, urlEncodeVars)
import           System.Exit (exitFailure)
-- import           System.Posix.Env.ByteString (getArgs)
import           System.Random (StdGen, mkStdGen, randomRs)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map

-- TODO: write some real tests instead of this hacky main
main :: IO ()
main = do
  let file = "/home/nabil/Desktop/10Day.torrent"
  contents <- BL.readFile file
  -- for a torrent file correctly encoded as described in the specification,
  -- `parse parseBencode contents` should always result in Done;
  -- but if the torrent file is incorrectly encoded, this code will error
  -- TODO: make the program handle this case gracefully
  let (Done remainder hash) = parse parseBencode contents
      url                   = constructURL (mkStdGen 42) hash
  case remainder of
    "" -> putStrLn ("CONTINUING; consumed all of torrent file " ++ file)
    _  -> putStrLn ("ABORTING; could not consume all of torrent file " ++ file)
          >> exitFailure
  putStrLn $ "making GET request to:\n" ++ url
  response <- simpleHTTP $ getRequest url
  putStrLn "the response from the server is:\n"
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
-- TODO: don't use fromJust -- if hash is bencoded but non-spec-conforming,
-- it is possible to cause an exception rather than gracefully handle an error
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
