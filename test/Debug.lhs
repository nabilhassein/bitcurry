I was testing some code I wrote, which can be found at:

https://github.com/nabilhassein/bitcurry

There is no need to examine the original code linked above; everything relevant
is contained in this literate Haskell file.

bitcurry is a bittorrent client. It communicates over the wire. As a result,
`ByteString` is the only text representation type we need to work with.

Internally we operate on the `Bencode` data structure, which represents
the data structure described in the bittorrent specification:
https://wiki.theory.org/BitTorrentSpecification#Bencoding

We need to antiParse and parse (i.e. serialize/deserialize, or encode/decode)
these structures because when we communicate over the wire, we send and receive
`ByteStrings`. No network function sends or receives `Bencode`s.

You can think about this a low level: we send and receive `ByteString`s because
we are using sockets.
You can also think about this at a high level: no standard network function has
a type signature allowing it accept an argument of type `Bencode`.

(Aside: I think I could write such a function easily after fixing this issue.)

Standard techniques to derive (de-)serialization code in an automated fashion
seem unsuited to my use case, because the bittorrent specification requires
its bytestrings to obey the conventions of a data format unique to bittorrent.

Since we need them, we first import Haskell's `ByteString`s. No problem here.

> import qualified Data.ByteString.Lazy as BL

On the other hand, I want to get rid of this next import.

> import Data.ByteString.Lazy.Char8 (pack)

Ugly! I'm working exclusively with ByteStrings -- why do I need a Char8?

I'm especially motivated because if I get rid of this import, I can change
from lazy to strict Haskell ByteStrings globally in my program by altering
only a single import statement. This seems very, very nice to have.

(In fact this seems almost like ML modules but that's beside the point.)

To get rid of this extraneous import, I changed my code from (1) to (2).
But there is a bug: (1) and (2) do not compute the same ByteString, as desired.

Below is my attempt to figure out why, based on a simplified code example.

What I think is the essence of the issue is the difference between ex1 and ex2:

> main :: IO ()
> main = do
>   putStrLn "ex1: "
>   print ex1
>   putStrLn "ex2: "
>   print ex2

Before I can introduce ex1 and ex2 I must specify the following:

> n :: Int
> n = 33

Aside:
  The number 33 is not special. A similar problem will occur with ANY number.
  However, it is important that `n` has type `Int`, because in my test code,
  I use Control.Monad.replicateM, which has type
  `replicateM :: Monad m => Int -> m a -> m [a]`

  `n` is passed as `replicateM`'s first argument because I need to define
  QuickCheck's `arbitrary` function on ByteString's in a very particular
  way, namely, that mandated by the bittorrent specification, linked above.

Now for the examples.

> ex1 :: BL.ByteString
> ex1 = (pack . show) n

As intended and expected, ex1 yields `Chunk "33" Empty`

> ex2 :: BL.ByteString
> ex2 = (BL.singleton . fromIntegral) n

Undesirably and unexpectedly, ex2 yields `Chunk "!" Empty`

This must be related to the fact that in ASCII, 33 maps to '!'.

How can I entirely avoid `Char`, but still compute `Chunk "33" Empty`?


-------------------------------------------------------------------------------

Here's a (hopefully unnecessary) longer version:

ex1 and ex2 are motivated by my desire to reify a representation
of the length of a ByteString. This is necessary because the bittorrent
specification, which bitcurry aims to eventually implement, states that
a bytestring is represented by its length as a base ten ASCII integer,
followed by a colon, followed by n bytes. There is no ending delimiter.
If so inclined you can examine the specification yourself, linked above.

Below find an excerpt from the real code, showing more context.
antiParse1 corresponds to ex1, antiParse2 to ex2.

> data Bencode = BString BL.ByteString

Really there are other constructors as well, but I omitted them here because
they are irrelevant. See my original code listing, linked above, if interested.

> antiParse1 :: Bencode -> BL.ByteString
> antiParse1 (BString s) = pack (show $ BL.length s)
>                              `BL.append` ":"
>                              `BL.append` s
>
> antiParse2 :: Bencode -> BL.ByteString
> antiParse2 (BString s) = (BL.singleton . fromIntegral $ BL.length s)
>                          `BL.append` ":"
>                          `BL.append` s

Now, the point of all this is that I prefer antiParse2, because it does not use
`Char8`s and `Strings` at all. I thought antiParse was a poor-taste hack.

Debugging question:
How can I correct antiParse2 so that it computes the same answer as antiParse1?

Design question:
Should I alter the Bencode data structure to explicitly store an
`Int` or `Integer` length field? Would that be more efficient or convenient?
