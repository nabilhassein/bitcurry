{-# LANGUAGE OverloadedStrings #-}

module PeerToPeer where

import Bencode                    (Bencode(BString, BInt, BList, BDict), Dict,
                                   getValue)
import TrackerClient              (getTorrentInfo, makeTrackerRequest,
                                   bytestring_info_hash, info_hash, peer_id)
import Data.ByteString.Lazy.Char8 (pack) -- instance IsString ByteString
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

encodeMessage :: Message ->      BL.ByteString
encodeMessage    KeepAlive     = BL.singleton 0
encodeMessage    Choke         = BL.singleton 1 `BL.append` BL.singleton 0
encodeMessage    Unchoke       = BL.singleton 1 `BL.append` BL.singleton 1
encodeMessage    Interested    = BL.singleton 1 `BL.append` BL.singleton 2
encodeMessage    NotInterested = BL.singleton 1 `BL.append` BL.singleton 3

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
getPeers :: Dict -> Maybe [(HostName, PortID)]
getPeers    dict  = case getValue "peers" dict of
  Just (BString binaryPeers) -> undefined
  Just (BList   hashedPeers) -> undefined
  _                          -> Nothing

-- the first message sent by a client to a peer
handshake :: StdGen -> Dict -> Maybe BL.ByteString
handshake    seed      dict  = case bytestring_info_hash dict of
  Nothing       -> Nothing
  Just infoHash -> Just $ foldr1 BL.append [ pstrlen
                                           , pstr
                                           , reserved
                                           , infoHash
                                           , peerID
                                           ]
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
