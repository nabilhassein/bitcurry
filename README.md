# about
bitcurry is to be a simple specification-compliant bittorrent client.
It is written in Haskell.

If this document fails to answer any questions you have about it,
please let me know!

# progress
So far, I've written a datatype for bencoding (src/Bencode.hs)
with associated code for parsing and inverse-parsing.

I've also written a draft of the module (src/TrackerClient.hs)
that sends a GET request to the tracker.

After that is complete and correct, we can begin implementing
peer-to-peer communication, and actually downloading files.

## possible issues to revisit
### lazy versus strict ByteStrings
Currently lazy ByteStrings are used universally in this project.

### encoding -- ASCII vs. Unicode
See https://en.wikipedia.org/wiki/Bencode#Encoding_algorithm

"specification does not deal with encoding of characters outside the ASCII set"

# TODOs
See also the TODOs in the code. (Meta: do TODOs belong in code?)

We need tests. Unfortunately, testing is not always straightforward.
However, it is a top priority to write good tests for:

1. Client-tracker interaction -- how can we know if the GET request is correct?
This is particularly tricky because it's possible to make a correct request and
receive an incorrect response -- or no response -- due to no fault of the client
(if the tracker has an error, cannot be found, the .torrent file is wrong, etc.)
2. Parsing ByteStrings into our Bencoded data structure
3. Anti-parsing our Bencoded data structure back into ByteStrings
4. Any code we write in the future -- preferably soon after, or even before

Informally testing the parser and anti-parser in the REPL was simple,
and apparently effective; they seem to work as specified.
However, it is not so simple to test the client-tracker interaction in this way.

So it's about time to get started with QuickCheck, HUnit, etc.

# getting started hacking bitcurry
## project initiation
If you want to contribute to bitcurry (all are welcome!), first make sure that
you have the [Haskell Platform](http://www.haskell.org/platform/) installed.

Then bring up a shell and:

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

If you use Emacs, make sure you get [virthualenv.el](https://github.com/Paczesiowa/virthualenv/blob/master/virthualenv.el),
put it on your `load-path` and `require` it in your `init.el`, and then

    M-x virthualenv-activate

every time you start working.
(Or do the equivalent in your environment of choice; for example,
if you're on the command-line, then repeat the incantation above, i.e.

    source .virthualenv/bin/activate

If you forget, your inferior ghci (or whatever) will be unable to find packages
that were `cabal install`-ed in the local virtual environment,
as ghci will instead search the normal environment.

Also, be sure to `:set -XOverloadedStrings` in your `.ghci`, `init.el`, etc.
Unfortunately ghci doesn't appear to respect language pragmas in files it loads.

If you have any questions -- or advice! -- about good practices and/or
setting up a productive development environment, please share!

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
