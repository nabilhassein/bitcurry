{-# LANGUAGE OverloadedStrings #-}

module TrackerClient (getTorrentInfo, makeTrackerRequest, info_hash,
                      bytestring_info_hash, peer_id) where

import Bencode
import BTError

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither, left)
import Data.Attoparsec.Lazy       (Result(Done), parse)
import Data.ByteString.Lazy.Char8 (pack, unpack) -- instance IsString ByteString
import Data.Digest.Pure.SHA       (sha1, showDigest, bytestringDigest)
import Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base          (urlEncodeVars)
import System.Random              (StdGen, randomRs)
import qualified Data.ByteString.Lazy as BL


getTorrentInfo :: FilePath -> EitherT BTError IO Dict
getTorrentInfo    filename  = do
  contents <- lift $ BL.readFile filename
  case parse parseBencode contents of
    Done "" (BDict d) -> do
      _ <- hoistEither $ getValue "info" d
      _ <- hoistEither $ getValue "announce" d
      return d
    _ -> left NoParse

makeTrackerRequest :: StdGen -> Dict -> EitherT BTError IO Dict
makeTrackerRequest    seed      dict  = do
  url      <- constructURL seed dict
  response <- lift $ simpleHTTP (getRequest url) >>= getResponseBody
  case parse parseBencode $ pack response of
    Done "" (BDict d) -> hoistEither (checkSuccess d) >> return d
    _                 -> left NoParse

-- note: urlEncodeVars strips the %s we encode below, hence need for special case
constructURL :: StdGen -> Dict -> EitherT BTError IO String
constructURL    seed      dict = do
  url        <- hoistEither $ getValue "announce" dict
  (key, val) <- hoistEither $ info_hash dict
  let strip    = drop 1 . dropWhile (/= ':') -- for bencoded strings
      baseURL  = strip . unpack . antiParse $ url
      infoHash = key ++ "=" ++ val
  return $ baseURL ++ "?" ++ infoHash ++ "&" ++
    urlEncodeVars [peer_id seed, port, uploaded, downloaded, left', compact, event]

-- % hackery is due to the requirements of the tracker server
info_hash :: Dict -> Either BTError (String, String)
info_hash    dict  = do
  d <- getValue "info" dict
  let hash :: String
      hash  = showDigest . sha1 $ antiParse d
      encodePercents :: String -> String
      encodePercents    []        = ""
      encodePercents    [_]       = fail "SHA1 hashes are 20 bytes"
      encodePercents    (a:b:str) = '%' : a : b : encodePercents str
  Right ("info_hash", encodePercents hash)

-- send to the peers over TCP; hence ByteString rather than String
bytestring_info_hash :: Dict -> Either BTError BL.ByteString
bytestring_info_hash    dict  = getValue "info" dict >>=
                                Right . bytestringDigest . sha1 . antiParse

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

left' :: (String, String)
left'  = ("left", "0")

-- TODO: research implications of compact (and no_peer_id)
compact :: (String   , String)
compact  = ("compact", "1")

-- TODO: alter this (so probably change type) based on actual events
event :: (String , String)
event  = ("event", "started")

-- TODO: ip, num_want, key, trackerid, no_peer_id (optional; omitted for now)
