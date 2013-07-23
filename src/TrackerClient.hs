{-# LANGUAGE OverloadedStrings #-}

module TrackerClient (getTorrentInfo, makeTrackerRequest, info_hash,
                      alt_info_hash, peer_id) where

import Bencode                    (Bencode(BString, BInt, BList, BDict), Hash,
                                   antiParse, parseBencode, getValue)
import Data.Attoparsec.Lazy       (Result(Done), parse)
import Data.ByteString.Lazy.Char8 (pack, unpack) -- instance IsString ByteString
import Data.Digest.Pure.SHA       (sha1, showDigest, bytestringDigest)
import Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base          (urlEncodeVars)
import System.Random              (StdGen, randomRs)
import qualified Data.ByteString.Lazy as BL

-- for a torrent file correctly encoded as described in the specification,
-- `parse parseBencode contents` always results in `Done "" (BDict hash)`;
-- but if the torrent file is incorrectly encoded, this code will error;
-- similarly for the response from the tracker in the next function below
-- TODO: make the program handle this case gracefully
getTorrentInfo :: FilePath -> IO (Maybe Hash)
getTorrentInfo    filename  = do
  contents <- BL.readFile filename
  let Done "" (BDict hash) = parse parseBencode contents
  case (getValue "info" hash, getValue "announce" hash) of
    (Just _, Just _) -> return $ Just hash
    _                -> return Nothing

-- TODO: can we make GET request with all ByteStrings? it's confusing to use
-- Strings here when ByteStrings are required for peer-to-peer communication
makeTrackerRequest :: StdGen -> Hash -> IO (Maybe Hash)
makeTrackerRequest    seed      hash = case constructURL seed hash of
  Nothing  -> return Nothing
  Just url -> do
    response <- getResponseBody =<< simpleHTTP (getRequest url)
    case parse parseBencode $ pack response of
      Done "" (BDict answer) -> return $ Just answer
      _                      -> return Nothing

alt_info_hash :: Hash -> Maybe BL.ByteString
alt_info_hash hash = getValue "info" hash >>=
                     Just . bytestringDigest . sha1 . antiParse

info_hash :: Hash -> Maybe (String, String)
info_hash    hash = case getValue "info" hash of
  Nothing -> Nothing
  Just dict ->
    let value = showDigest . sha1 . antiParse $ dict
      -- only called on 20-byte SHA1 hashes, so odd case never happens
      -- TODO: make this less appallingly hacky
      -- check sha1 library for a way to encode '%'s as binary instead of hex
        encodePercents ""        = ""
        encodePercents (a:b:str) = '%' : a : b : encodePercents str
    in Just ("info_hash", encodePercents value)

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

-- TODO: research implications of compact (and no_peer_id)
compact :: (String, String)
compact  = ("compact", "1")

-- TODO: alter this (so probably change type) based on actual events
event :: (String, String)
event  = ("event", "started")

-- TODO: ip, num_want, key, trackerid, no_peer_id (optional; omitted for now)

-- TODO: handle info_hash in standard manner instead of as a hacky special case
constructURL :: StdGen -> Hash -> Maybe String
constructURL    seed      hash =
  case (getValue "announce" hash, info_hash hash) of
    (Just dict, Just ans) ->
      let strip    = drop 1 . dropWhile (/= ':') -- for bencoded strings
          baseURL  = strip . unpack . antiParse $ dict
          infoHash = fst ans ++ "=" ++ snd ans
      in  Just $ baseURL ++ "?" ++ infoHash ++ "&" ++
          urlEncodeVars [ peer_id seed
                        , port
                        , uploaded
                        , downloaded
                        , left
                        , compact
                        , event
                        ]
    _                     -> Nothing
