# about
bitcurry is to be a simple specification-compliant bittorrent client.
It is written in Haskell.

If this document fails to answer any questions you have about it,
please let me know!

# current status
So far, I've written a datatype for bencoding (src/Bencode.hs)
with associated code for parsing and anti-parsing.

I've also written a draft of the module (src/TrackerClient.hs)
that sends an HTTP GET request to the tracker. This works as intended.

I've taken the very first steps re: peer-to-peer communication.
The idea is to implement each TCP connection to a peer in its own thread.
Haskell uses green threads so since the number of peers a client is actively
engaged in downloading from is usually in the double digits,
the number of threads this approach entails is very easily handled.

There is no need for one TCP connection to have direct knowledge of another.
Synchronization is, however, necessary for the following:  
  - logging to stdout, stderr, or a file  
  - writing to the file the client is trying to download from its peers  
  - "playing games" or "dancing" with peers to get files according to a
    hopefully effective algorithmic/game theoretic strategy

One master thread communicating with all of its children should be a simple
and effective model.

The idea is to have a queue of pieces; a thread tries to download a piece
from the queue; the download is complete when the queue is empty.

I've begun writing tests of the pure code. Greater coverage is needed, but a
higher priority than testing pure code is to devise a good method to test
networked communication.

# TODOs
Figure out modules: what to expose as a library, and how?

String encoding and representation: sort out ASCII vs. UTF-8.
In terms of Haskell types, there is Char, String, Text, Word8, and ByteString,
as well as lazy vs strict considerations, esp. w.r.t. ByteStrings.
Consider http://hackage.haskell.org/cgi-bin/hackage-scripts/package/utf8-string
in addition to the currently imported modules.

See also the TODOs in the code.

## testing
We need tests. Unfortunately, testing is not always straightforward.
However, it is a top priority to write good tests for:

1. Network communication -- given that errors are possible over the wire
and in trackers, peers, etc., how can we know if our code is correct?
It's possible to make a correct request and receive an incorrect response --
 or no response -- due to no fault of the client.
2. Parsing ByteStrings into our Bencoded data structure
3. Anti-parsing our Bencoded data structure back into ByteStrings

## future features

### bittorrent enhancement proposals
BEP 15 is a particular priority: implementing ability to communicate with
trackers over UDP. Useful services like openbittorrent use this.

### creating torrent files
bitcurry ought to offer a convenient command-line interface to create torrents,
so that it is useful for upload as well as download.


# getting started hacking bitcurry
## project initiation
If you want to contribute to bitcurry (all are welcome!), first make sure that
you have the [Haskell Platform](http://www.haskell.org/platform/) installed.
(It is necessary to use GHC 7.6 for this project; bitcurry uses some functions
and language features that are new to this version.)

Then just bring up a shell and:

    git clone git@github.com:nabilhassein/bitcurry.git
    cd bitcurry
    cabal install --only-dependencies && cabal configure --enable-tests && cabal build

If all is right with the world, this will build the package.

*PLEASE let me know if this does not work for you.*

## directory annoyances

Be sure to stay inside of the `bitcurry` directory. If you `cd` into `src`,
 `test`, etc. you may encounter irritating errors like:

    "Could not find module Bencode"

due to the mundane details of ghc's (and ghci's) resolution of `import`s.

I've tried to hack around this, not only in `bitcurry.cabal` but also in the
project's local `.ghci`, to apparent success. Let me know if you have trouble.

## contributing

In terms of actually writing code: for now, I guess you should
develop on a local branch and then send me a pull request.
If you want to begin working on this project regularly, or even semi-regularly,
I'll gladly give commit access! Just ask.

Currently, writing tests would be especially helpful.

# useful resources and references
- https://wiki.theory.org/BitTorrentSpecification
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-2/
- https://github.com/thomasballinger/bittorrent
- https://github.com/astro/haskell-torrent
- http://www.haskell.org/pipermail/haskell-cafe/2005-April/009638.html
- http://jlouisramblings.blogspot.com/2010/01/state-of-haskell-torrent.html
- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
