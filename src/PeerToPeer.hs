{-# LANGUAGE OverloadedStrings #-}

module PeerToPeer where

import Bencode                    (Bencode(BString, BInt, BList, BDict), Dict,
                                   getValue)
import TrackerClient              (getTorrentInfo, makeTrackerRequest,
                                   bytestring_info_hash, info_hash, peer_id)
import Data.ByteString.Lazy.Char8 (pack, unpack) -- instance IsString ByteString
import Data.List.Split            (chunksOf)
import Data.Monoid                ((<>))
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

-- note: using fromIntegral :: Int -> Word8 computes an answer modulo 256
lengthPrefix :: Int -> BL.ByteString
lengthPrefix         = BL.pack . padAndReverse . bytes
  where bytes :: Int -> [Word8] -- little endian
        bytes    0    = []
        bytes    n    = (fromIntegral (n `mod` (2^8))) : bytes (n `div` (2^8))
        padAndReverse   :: [Word8] ->         [Word8]
        padAndReverse      []               = [0 , 0 , 0 , 0 ]
        padAndReverse      [b1]             = [0 , 0 , 0 , b1]
        padAndReverse      [b1, b2]         = [0 , 0 , b2, b1]
        padAndReverse      [b1, b2, b3]     = [0 , b3, b2, b1]
        padAndReverse      [b1, b2, b3, b4] = [b4, b3, b2, b1]
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

-- from the spec: "the peers value may be a string consisting of multiples of 6
-- bytes. First 4 bytes are the IP address and last 2 bytes are the port number.
-- All in network (big endian) notation"
--getPeers :: Dict -> Maybe [(HostName, PortID)]
getPeers    dict  = case getValue "peers" dict of
  Just (BString binaryPeers) ->
    let addrs = chunksOf 6 $ unpack binaryPeers
        ips   = map (take 4) addrs
        ports = map (drop 4) addrs
    in  undefined
  Just (BList   hashedPeers) -> undefined
  _                          -> undefined

-- the first message sent by a client to a peer
handshake :: StdGen -> Dict -> Maybe BL.ByteString
handshake    seed      dict  = bytestring_info_hash dict >>= \ infoHash ->
  Just $ foldr1 BL.append [pstrlen, pstr, reserved, infoHash, peerID]
  where pstrlen   = BL.singleton 19
        pstr      = "BitTorrent protocol"
        reserved  = "\0\0\0\0\0\0\0\0"
        peerID    = pack $ snd $ peer_id seed


main :: IO ()
main = undefined
-- sketch of flow:
-- Read torrent file.
-- If necessary info is extracted, make GET request to tracker; otherwise abort.
-- Now extract list of peers from tracker's HTTP response; if impossible, abort.
-- Choose maxPeers peers; fork a thread to open a TCP connection with each.
-- Each of these threads should maintain internal state, and act accordingly:
-- https://wiki.theory.org/BitTorrentSpecification#Peer_wire_protocol_.28TCP.29
-- The master thread should keep track of any relevant state information for all
-- child threads, as well as piecing together the downloaded file.
