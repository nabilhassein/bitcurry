{-# LANGUAGE OverloadedStrings #-}

module TrackerClient (getTorrentInfo, makeTrackerRequest, info_hash,
                      bytestring_info_hash, peer_id) where

import Bencode                    (Bencode(BString, BInt, BList, BDict), Dict,
                                   antiParse, parseBencode, getValue)
import Data.Attoparsec.Lazy       (Result(Done), parse)
import Data.ByteString.Lazy.Char8 (pack, unpack) -- instance IsString ByteString
import Data.Digest.Pure.SHA       (sha1, showDigest, bytestringDigest)
import Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base          (urlEncodeVars)
import System.Random              (StdGen, randomRs)
import qualified Data.ByteString.Lazy as BL


getTorrentInfo :: FilePath -> IO (Maybe Dict)
getTorrentInfo    filename  = do
  contents <- BL.readFile filename
  case parse parseBencode contents of
    Done "" (BDict d) -> case (getValue "info" d, getValue "announce" d) of
      (Just _, Just _) -> return $ Just d
      _                -> return Nothing
    _ -> return Nothing

makeTrackerRequest :: StdGen -> Dict -> IO (Maybe Dict)
makeTrackerRequest    seed      dict  = case constructURL seed dict of
  Nothing  -> return Nothing
  Just url -> do
    response <- getResponseBody =<< simpleHTTP (getRequest url)
    case parse parseBencode $ pack response of
      Done "" (BDict d) -> case getValue "failure reason" dict of
        Nothing -> return $ Just d
        Just _  -> return Nothing
      _                 -> return Nothing

-- TODO: handle info_hash in standard manner instead of as a hacky special case
-- this is so currently because urlEncodeVars strips the %s we encoded above
constructURL :: StdGen -> Dict -> Maybe String
constructURL    seed      dict =
  case (getValue "announce" dict, info_hash dict) of
    (Just url, Just (key, val)) ->
      let strip    = drop 1 . dropWhile (/= ':') -- for bencoded strings
          baseURL  = strip . unpack . antiParse $ url
          infoHash = key ++ "=" ++ val
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

-- % hackery is due to the requirements of the tracker server
info_hash :: Dict -> Maybe (String, String)
info_hash    dict  = getValue "info" dict >>= \ d ->
  let hash :: String
      hash  = showDigest . sha1 $ antiParse d
      encodePercents :: String -> String
      encodePercents    []        = ""
      encodePercents    [_]       = fail "fatal error: SHA1 hashes are 20 bytes"
      encodePercents    (a:b:str) = '%' : a : b : encodePercents str
  in Just ("info_hash", encodePercents hash)

-- send to the peers over TCP, hence ByteString rather than String
-- not used in this module but it logically belongs here
bytestring_info_hash :: Dict -> Maybe BL.ByteString
bytestring_info_hash    dict  = getValue "info" dict >>=
                                Just . bytestringDigest . sha1 . antiParse

-- from the specification linked above: "[the peer id] must at least be unique
-- for your local machine, thus should probably incorporate things like
-- process ID and perhaps a timestamp recorded at startup"
-- TODO: change seed generation to incorporate advice of above quote
peer_id :: StdGen -> (String   , String)
peer_id    seed    = ("peer_id", "-HS0001-" ++ twelveRandomDigits)
  where twelveRandomDigits = take 12 (randomRs ('0', '9') seed)

-- TODO: have this try 6882 - 6889 in the case of a port already in use
port :: (String, String)
port  = ("port", "6881")

-- TODO: have uploaded, downloaded, left actually report meaningful data
uploaded :: (String    , String)
uploaded  = ("uploaded", "0")

downloaded :: (String      , String)
downloaded  = ("downloaded", "0")

left :: (String, String)
left  = ("left", "0")

-- TODO: research implications of compact (and no_peer_id)
compact :: (String   , String)
compact  = ("compact", "1")

-- TODO: alter this (so probably change type) based on actual events
event :: (String , String)
event  = ("event", "started")

-- TODO: ip, num_want, key, trackerid, no_peer_id (optional; omitted for now)
