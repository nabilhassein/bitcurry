# about
bitcurry is to be a simple specification-compliant bittorrent client.
It is written in Haskell.

If this document fails to answer any questions you have about it,
please let me know!

# getting started

## project initiation
If you want to pitch in (all are welcome!), bring up a terminal and:

    git clone git@github.com:nabilhassein/bitcurry.git
    cd bitcurry
    cabal install virthualenv
    virthualenv

Follow the above steps the FIRST time you begin working on the project.

Then, EVERY time before you do work such as editing code, or
(especially!) `cabal install`-ing dependencies:

    source .virthualenv/bin/activate

This will give the project a virtual environment, avoiding dependency hell (I think).

(See http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)

## workflow

If you use Emacs, make sure you get [virthualenv.el](https://github.com/Paczesiowa/virthualenv/blob/master/virthualenv.el), and

    M-x virthualenv-activate

every time you start working.
(Or do the equivalent in your environment of choice; for example,
if you're on the command-line, then repeat the incantation above, i.e.

    source .virthualenv/bin/activate

If you forget, your inferior ghci (or whatever) will be unable to find packages
that were `cabal install`-ed in the local virtual environment,
as ghci will instead search the normal environment.

Also, be sure to `:set -XOverloadedStrings` in your `.ghci`.

## contributing

In terms of actually writing code: for now, I guess you should
develop on a local branch and then send me a pull request.
If you want to begin working on this project regularly, or even semi-regularly,
I'll gladly give commit access! Just ask.

### meta

This isn't about bitcurry strictly, but if you have any ideas about better
development practices, whether general or haskell-specific, please let me know!

# TODO
See also the TODOs in the code.

We need tests. Unfortunately, testing is not always straightforward.
To be continued.

So far, I've written a datatype for bencoding,
with associated code for parsing and serialization.

The next step I plan to tackle is [communicating with the tracker via HTTP](https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol).

After that, we can begin working on implementing peer-to-peer communication,
and actually downloading files.

As an additional feature not found in the specification, I hope to implement
the ability to control the client via SMS. This has nothing in particular to do
with Bittorrent.

## possible issues to revisit

### encoding -- ASCII vs. Unicode
See https://en.wikipedia.org/wiki/Bencode#Encoding_algorithm

"The specification does not deal with encoding of characters outside the ASCII set"

# useful resources and references
- https://wiki.theory.org/BitTorrentSpecification
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/
- http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-2/
- https://github.com/thomasballinger/bittorrent
- https://github.com/astro/haskell-torrent
- http://www.haskell.org/pipermail/haskell-cafe/2005-April/009638.html
