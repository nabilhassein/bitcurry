# about
bitcurry is to be a simple specification-compliant bittorrent client.
It is written in Haskell.

# project initiation
    cabal install virthualenv
    cd /path/to/bitcurry
    virthualenv
    source .virthualenv/bin/activate

Follow these steps before you cabal install anything needed for the project.
In this way dependency hell can be avoided...I think.
See http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

# Useful resources and references
- https://wiki.theory.org/BitTorrentSpecification
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-2/
- http://hackage.haskell.org/package/attoparsec-0.10.4.0
- https://github.com/thomasballinger/bittorrent
- https://github.com/astro/haskell-torrent
- http://www.haskell.org/pipermail/haskell-cafe/2005-April/009638.html

# TODO
Everything!

## Issues to revisit

### Encoding -- ASCII vs. Unicode
See https://en.wikipedia.org/wiki/Bencode#Encoding_algorithm

"The specification does not deal with encoding of characters outside the ASCII set"


## Starting out
The first link in the "resources" section above is the most important:
it is a specification of the bittorrent protocol.
The first thing I plan to do is write the tracker:
https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol

According to that link, "The tracker is an HTTP/HTTPS service which responds to HTTP GET requests."
I'm not exactly a pro web hacker but that seems like the easiest place to start.

This is why the initial commit makes use of [Scotty](http://hackage.haskell.org/packages/archive/scotty/0.4.0/doc/html/Web-Scotty.html).


## Meta
get virthualenv, emacs's haskell-mode, and git all playing nicely
