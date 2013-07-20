{-# LANGUAGE OverloadedStrings #-}

module TrackerClient (getTorrentInfo, getValue, makeTrackerRequest, info_hash,
                      alt_info_hash, peer_id) where

import Prelude hiding (lookup)

import Bencode                    (Bencode(BString, BInt, BList, BDict), Hash,
                                   antiParse, parseBencode)
import Data.Attoparsec.Lazy       (Result(Done), parse)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Digest.Pure.SHA       (sha1, showDigest, bytestringDigest)
import Data.Map                   (lookup)
import Data.Maybe                 (fromJust)
import Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base          (urlEncodeVars)
import System.Random              (StdGen, mkStdGen, randomRs)
import qualified Data.ByteString.Lazy as BL

-- TODO: can we make GET request with all ByteStrings? it's confusing to use
-- Strings here when ByteStrings are required for peer-to-peer communication
main :: IO Hash
main  = getTorrentInfo "test/archlinux.torrent" >>=
        makeTrackerRequest (mkStdGen 42)

-- for a torrent file correctly encoded as described in the specification,
-- `parse parseBencode contents` always results in `Done "" (BDict hash)`;
-- but if the torrent file is incorrectly encoded, this code will error;
-- similarly for the response from the tracker in the next function below
-- TODO: make the program handle this case gracefully
getTorrentInfo :: FilePath -> IO Hash
getTorrentInfo    filename  = do
  contents <- BL.readFile filename
  let Done "" (BDict hash) = parse parseBencode contents
  return hash

makeTrackerRequest :: StdGen -> Hash -> IO Hash
makeTrackerRequest    seed      hash = do
  let url = constructURL seed hash
  response <- getResponseBody =<< simpleHTTP (getRequest url)
  let Done "" (BDict answer) = parse parseBencode $ pack response
  return answer

-- TODO: stop using fromJust and handle errors gracefully instead
getValue :: BL.ByteString -> Hash -> Bencode
getValue    key                    = fromJust . lookup key

alt_info_hash :: Hash -> BL.ByteString
alt_info_hash hash = bytestringDigest . sha1 . antiParse $ getValue "info" hash

info_hash :: Hash -> (String, String)
info_hash    hash =
  let value = showDigest . sha1 . antiParse $ getValue "info" hash
      -- only called on 20-byte SHA1 hashes, so odd case never happens
      -- TODO: make this less appallingly hacky
      -- check sha1 library for a way to encode '%'s as binary instead of hex
      encodePercents ""        = ""
      encodePercents (a:b:str) = '%' : a : b : encodePercents str
  in ("info_hash", encodePercents value)

-- from the specification linked above: "[the peer id] must at least be unique
-- for your local machine, thus should probably incorporate things like
-- process ID and perhaps a timestamp recorded at startup"
-- TODO: change seed generation to incorporate advice of above quote
peer_id :: StdGen -> (String, String)
peer_id    seed    = ("peer_id", "-HS0001-" ++ twelveRandomDigits)
  where twelveRandomDigits = take 12 (randomRs ('0', '9') seed)

-- TODO: have this try 6882 - 6889 in the case of a port already in use
port :: (String, String)
port  = ("port", "6881")

-- TODO: have uploaded, downloaded, left actually report meaningful data
uploaded :: (String, String)
uploaded  = ("uploaded", "0")

downloaded :: (String, String)
downloaded  = ("downloaded", "0")

left :: (String, String)
left  = ("left", "0")

-- TODO: research implications of compact
compact :: (String, String)
compact  = ("compact", "1")

-- no_peer_id omitted because compact=1 (see specification)

-- TODO: alter this (so probably change type) based on actual events
event :: (String, String)
event  = ("event", "started")

-- TODO: include ip, num_want, key, trackerid (optional; temporarily omitted)

-- construct url to use in GET request to tracker
-- TODO: handle info_hash in standard manner instead of as a hacky special case
constructURL :: StdGen -> Hash -> String
constructURL    seed      hash =
  let strip    = drop 1 . dropWhile (/= ':') -- for bencoded strings
      baseURL  = strip . unpack . antiParse $ getValue "announce" hash
      infoHash = fst (info_hash hash) ++ "=" ++ snd (info_hash hash)
  in  baseURL ++ "?" ++ infoHash ++ "&" ++
      urlEncodeVars [ peer_id seed
                    , port
                    , uploaded
                    , downloaded
                    , left
                    , compact
                    , event
                    ]
