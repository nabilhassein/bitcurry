{-# LANGUAGE OverloadedStrings #-}

module PeerToPeer where

import BTError
import Bencode
import TrackerClient

import Control.Monad              (forM)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)
import Data.ByteString.Lazy.Char8 (pack, unpack) -- instance IsString ByteString
import Data.Monoid                ((<>))
import GHC.Int                    (Int64)
import GHC.Word                   (Word8)
import Network                    (HostName, PortID(..), connectTo)
import System.Random              (StdGen, mkStdGen)
import qualified Data.ByteString.Lazy as BL


-- TODO: for messages with payloads, add more fields
data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have
             | Bitfield
             | Request
             | Piece
             | Cancel
             | Port

-- from the spec: messages in the protocol take the form of
-- <length prefix><message ID><payload>
-- The length prefix is a four byte big-endian value.
-- The message ID is a single decimal byte.
-- The payload is message dependent. 

-- ?: is this encoding the bytes as decimals? is that what the spec even needs?
-- TODO: settle this question and write a test
-- note: using fromIntegral :: Int -> Word8 computes an answer modulo 256
-- this is intended behavior
lengthPrefix :: Int -> BL.ByteString
lengthPrefix         = BL.pack . padAndReverse . bytes
  where bytes :: Int -> [Word8] -- little endian
        bytes    0    = []
        bytes    n    = fromIntegral n : bytes (n `div` 2^8)
        padAndReverse   :: [Word8] ->         [Word8]
        padAndReverse      []               = [0 , 0 , 0 , 0 ]
        padAndReverse      [b1]             = [0 , 0 , 0 , b1]
        padAndReverse      [b2, b1]         = [0 , 0 , b1, b2]
        padAndReverse      [b3, b2, b1]     = [0 , b1, b2, b3]
        padAndReverse      [b4, b3, b2, b1] = [b1, b2, b3, b4]
        padAndReverse      _ = error "length must be < 2^32 to fit in 4 bytes"

encodeMessage :: Message ->      BL.ByteString
encodeMessage    KeepAlive     = lengthPrefix 0
encodeMessage    Choke         = lengthPrefix 1 <> BL.singleton 0
encodeMessage    Unchoke       = lengthPrefix 1 <> BL.singleton 1
encodeMessage    Interested    = lengthPrefix 1 <> BL.singleton 2
encodeMessage    NotInterested = lengthPrefix 1 <> BL.singleton 3

decodeMessage :: BL.ByteString -> Maybe Message
decodeMessage = undefined


-- see "Implementer's Note" from the unofficial specification:
-- https://wiki.theory.org/BitTorrentSpecification#Tracker_Response
maxPeers :: Int
maxPeers  = 30

-- arbitrary. TODO: incorporate time or randomness
globalSeed :: StdGen
globalSeed  = mkStdGen 42

-- currently this function only sends and receives the handshake
-- TODO: is it practical to implement the client's "dance" w/peer as a FSM?
downloadFromPeer :: HostName -> PortID -> BL.ByteString -> IO BL.ByteString
downloadFromPeer    host        port      handshakeMsg   = do
  handle <- connectTo host port
  BL.hPut handle handshakeMsg
  BL.hGet handle 68

-- see the spec: peers can be dictionary model or binary model. excerpts:
-- DICT: The value is a list of dictionaries, each with the following keys:
--   peer id: self-selected, as described above for the tracker request (string)
--   ip: IP address; IPv6 (hexed) or IPv4 (dotted quad) or DNS name (string)
--   port: peer's port number (integer) 
-- BINARY: Instead of using the dictionary model described above,
--   the peers value may be a string consisting of multiples of 6 bytes.
--   First 4 bytes are the IP address and last 2 bytes are the port number.
--   All in network (big endian) notation. 
getPeers :: Dict -> Either BTError [(HostName, PortID)]
getPeers    dict  = do
  peers <- getValue "peers" dict
  case peers of
    BList hashedPeers -> forM hashedPeers $ \(BDict peer) -> do
      BString ip   <- getValue "ip" peer
      BInt    port <- getValue "port" peer
      return (unpack ip, PortNumber $ fromIntegral port)
    BString binaryPeers ->
      let rawPeers    :: [BL.ByteString]
          rawPeers     = byteStringChunksOf 6 binaryPeers
          bytesToPort :: BL.ByteString -> Either BTError PortID
          bytesToPort    bs             = case BL.unpack bs of
            [b1, b2] -> let i1, i2 :: Int
                            i1 = fromIntegral b1
                            i2 = fromIntegral b2
                        in  Right $ PortNumber $ fromIntegral $ i1*2^8 + i2
            _        -> Left NoParse
      in forM rawPeers $ \peer -> do
        portID <- bytesToPort $ BL.drop 4 peer
        return (unpack . BL.take 4 $ peer, portID)
    _                   -> Left NoParse
    where
      byteStringChunksOf :: Int64 -> BL.ByteString -> [BL.ByteString]
      byteStringChunksOf    n        str            =
        if BL.null str
        then []
        else BL.take n str : byteStringChunksOf n (BL.drop n str)


-- the first message sent by a client to a peer
handshake :: StdGen -> Dict -> Either BTError BL.ByteString
handshake    seed      dict  = bytestring_info_hash dict >>= \ infoHash ->
  Right $ foldr1 BL.append [pstrlen, pstr, reserved, infoHash, peerID]
  where pstrlen   = BL.singleton 19
        pstr      = "BitTorrent protocol"
        reserved  = "\0\0\0\0\0\0\0\0"
        peerID    = pack $ snd $ peer_id seed


-- sketch of flow:
-- Read torrent file.
-- If necessary info is extracted, make GET request to tracker; otherwise abort.
-- Now extract list of peers from tracker's HTTP response; if impossible, abort.
-- Choose maxPeers peers; fork a thread to open a TCP connection with each.
-- Each of these threads should maintain internal state, and act accordingly:
-- https://wiki.theory.org/BitTorrentSpecification#Peer_wire_protocol_.28TCP.29
-- The master thread should keep track of any relevant state information for all
-- child threads, as well as piecing together the downloaded file.
flow :: FilePath -> EitherT BTError IO ()
flow    filename  = do
  dict     <- getTorrentInfo filename
  response <- makeTrackerRequest globalSeed dict
  peers    <- hoistEither $ getPeers response
  msg      <- hoistEither $ handshake globalSeed dict
  lift $ print response
  lift $ print peers

main :: IO (Either BTError ())
main = runEitherT $ flow "test/archlinux.torrent"
