{-# LANGUAGE OverloadedStrings #-}

module TrackerClient where

import           Bencode (Bencode(..), antiParse, parseBencode)
import           Data.Attoparsec.Lazy (Result(..), parse)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.Char (ord)
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Data.IP (IPv4, toIPv4)
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromJust)
import           Network (PortID(..))
import           Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Base (urlEncodeVars)
import           System.Random (StdGen, mkStdGen, randomRs)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map

main :: IO [(IPv4, PortID)]
main = do
  let file = "test/archlinux.torrent"
      seed = mkStdGen 42
  contents <- BL.readFile file
  -- for a torrent file correctly encoded as described in the specification,
  -- `parse parseBencode contents` should always result in Done;
  -- but if the torrent file is incorrectly encoded, this code will error
  -- TODO: make the program handle this case gracefully
  let Done "" contentsHash = parse parseBencode contents
      url                  = constructURL seed contentsHash
  putStrLn $ "making GET request to:\n" ++ url
  response <- simpleHTTP $ getRequest url
  putStrLn "the raw response from the server is:"
  foo <- getResponseBody response
  putStrLn foo
  let Done "" responseHash = parse parseBencode (pack foo)
      BString rawpeers     = getValue "peers" responseHash
      peers                = chunksOf 6 $ unpack rawpeers
      rawAddresses         = map (take 4) peers
      rawPorts             = map (drop 4) peers
      addresses            = map toIPv4 $ map (map ord) rawAddresses
      ports                = map helper $ map (map ord) rawPorts
  putStrLn "parsed this response as:"
  return $ zip addresses ports

helper :: [Int] -> PortID
helper [x, y] = PortNumber $ toEnum $ (x * 2^8) + y -- concatenate two bytes
helper _      = error "invalid format for port number"

-- TODO: don't use fromJust -- if hash is bencoded but non-spec-conforming,
-- it is possible to cause an exception rather than gracefully handle an error
getValue :: BL.ByteString -> Bencode -> Bencode
getValue key (BDict hash) = fromJust $ Map.lookup (BString key) hash
getValue _   _            = error "getValue: hash param is not a BDict"

-- parameters for GET request to tracker
-- https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol

info_hash :: Bencode -> (String, String)
info_hash hash =
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
-- TODO: handle info_hash in standard manner instead of as a hacky special case
constructURL :: StdGen -> Bencode -> String
constructURL seed hash =
  let strip   = drop 1 . dropWhile (/= ':') -- for bencoded strings
      baseURL = strip . unpack . antiParse $ getValue "announce" hash
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
