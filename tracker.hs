{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

-- https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol
-- tracker a good starting place? learn a bit of HTTP processing?

main = scotty 3000 $ do
  get "/" $ do
    html "soon enough I'll respond to requests for .torrent files"
