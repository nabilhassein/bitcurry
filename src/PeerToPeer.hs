{-# LANGUAGE OverloadedStrings #-}

module PeerToPeer where

import Bencode                    (Bencode(BString, BInt, BList, BDict), Hash)
import TrackerClient              (getTorrentInfo, getValue, makeTrackerRequest,
                                   alt_info_hash, info_hash, peer_id)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char                  (ord)
import Data.IP                    (IPv4, toIPv4)
import Data.List.Split            (chunksOf)
import Network                    (PortID(..), connectTo)
import System.Random              (StdGen, mkStdGen)
import qualified Data.ByteString.Lazy as BL


main :: IO BL.ByteString
main = do
  torrentInfo <- getTorrentInfo "test/archlinux.torrent"
  hash        <- makeTrackerRequest (mkStdGen 42) torrentInfo
  let peers = getPeers hash
      ip    = show $ fst $ head peers
      port  = snd $ head peers
      msg   = handshake (mkStdGen 42) torrentInfo
  putStrLn $ "handshake to " ++ ip ++ " " ++ show port ++ ":\n" ++ show msg
  putStrLn $ "this message has length " ++ show (BL.length msg)
  handle <- connectTo ip port
  BL.hPut handle msg
  BL.hGet handle 68

-- from the spec: "the peers value may be a string consisting of multiples of 6
-- bytes. First 4 bytes are the IP address and last 2 bytes are the port number.
-- All in network (big endian) notation"
-- TODO: deal with DNS, IPv6, other way to encode peers (i.e. list of dicts)
getPeers :: Hash -> [(IPv4, PortID)]
getPeers hash =
  let BString rawPeers = getValue "peers" hash
      peers            = chunksOf 6 $ unpack rawPeers
      addresses        = map (toIPv4 . map ord . take 4) peers
      ports            = map (concatTwoBytes . map ord . drop 4) peers
  in zip addresses ports
  where concatTwoBytes :: [Int] -> PortID
        concatTwoBytes [x, y] = PortNumber $ toEnum $ (x * 2^8) + y
        concatTwoBytes _      = error "invalid format for port number"


handshake :: StdGen -> Hash -> BL.ByteString
handshake seed hash = foldr1 BL.append [ pstrlen
                                       , pstr
                                       , reserved
                                       , infoHash
                                       , peerID
                                       ]
  where pstrlen   = BL.singleton 19
        pstr      = "BitTorrent protocol"
        reserved  = "\0\0\0\0\0\0\0\0"
        infoHash  = alt_info_hash hash
        peerID    = pack $ snd $ peer_id seed
