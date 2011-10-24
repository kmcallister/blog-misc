On the suggestion of [a
friend](http://wealoneonearth.blogspot.com/search/label/quasicrystal), I
rendered this animation:

[image]

This [quasicrystal](http://en.wikipedia.org/wiki/Quasicrystal) is full of
emergent patterns, but it can be described in a simple way.  Imagine that every
point in the plane is shaded according to the cosine of its *y* coordinate.
The result would look like this:

Now we can rotate this image to get other waves, like these:

Each frame of the animation is a summation of such waves at evenly-spaced
rotations.  The animation occurs as each wave moves forward.

I recommend viewing it up close, and then from a few feet back.  There are
different patterns at each spatial scale.


The code
========

To render this animation I wrote a Haskell program, using the
[Repa](http://repa.ouroborus.net/) array library.  For my purposes, the
advantages of Repa are:

* Immutable arrays, supporting clean, expressive code
* A fast implementation, including automatic parallelization
* Easy output to image files, via [`repa-devil`](http://hackage.haskell.org/package/repa-devil)

Here is a simplified (but complete!) program, which renders a single still
image.

> import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..) )
> import qualified Data.Array.Repa          as R
> import qualified Data.Array.Repa.IO.DevIL as D
>
> import Data.Word  ( Word8   )
> import Data.Fixed ( divMod' )

For clarity, we define a few type synonyms:

> type R     = Float
> type R2    = (R, R)
> type Angle = R

We'll convert pixel indices to coordinates in the real plane, with origin at
the image center.  We have to decide how many pixels to draw, and how much of
the plane to show.

> pixels :: Int
> pixels = 800
> scale :: R
> scale = 128

Repa's array indices are "snoc lists" of the form `(Z :. x :.  y)`.  By
contrast, our planar coordinates are conventional tuples.

> point :: DIM2 -> R2
> point = \(Z :. x :. y) -> (adj x, adj y) where
>     adj n = scale * ((2 * fromIntegral n / denom) - 1)
>     denom = fromIntegral pixels - 1

A single wave is a cosine depending on *x* and *y* coordinates in some
proportion, determined by the wave's orientation angle.

> wave :: Angle -> R2 -> R
> wave th = f where
>     (cth, sth) = (cos th, sin th)
>     f (x,y) = (cos (cth*x + sth*y) + 1) / 2

To combine several functions, we sum their outputs, and wrap to produce a
result between 0 and 1.  As `n` increases, `(wrap n)` will rise to 1, fall back
to 0, rise again, and so on.  `sequence` converts a list of functions to a
function returning a list, using the monad instance for `((->) r)`.

> combine :: [R2 -> R] -> (R2 -> R)
> combine xs = wrap . sum . sequence xs where
>     wrap n = case divMod' n 1 of
>         (k, v) | odd k     -> 1-v
>                | otherwise -> v

To draw the quasicrystal, we combine waves at 7 angles evenly spaced between 0
and *&pi;*.

> angles :: Int -> [Angle]
> angles n = take n $ enumFromThen 0 (pi / fromIntegral n)
>
> quasicrystal :: DIM2 -> R
> quasicrystal = combine (map wave (angles 7)) . point

We convert an array of floating-point values to an image in two steps.  First,
we map floats in [0,1] to bytes in [0,255].  Then we copy this to every color
channel.  The result is a 3-dimensional array, indexed by (row, column,
channel).  `repa-devil` takes such an array and outputs a PNG image file.

> toImage :: Array DIM2 R -> Array DIM3 Word8
> toImage arr = R.traverse arr8 (:. 4) chans where
>     arr8 = R.map (floor . (*255) . min 1 . max 0) arr
>     chans _ (Z :. _ :. _ :. 3) = 255  -- alpha channel
>     chans a (Z :. x :. y :. _) = a (Z :. x :. y)
>
> main :: IO ()
> main = do
>     let arr = R.fromFunction (Z :. pixels :. pixels) quasicrystal
>     D.runIL $ D.writeImage "out.png" (toImage arr)


Running it
==========

Repa's array operations automatically run in parallel.  We just need to enable
GHC's threaded runtime.

~~~~
$ ghc -O2 -rtsopts -threaded quasicrystal.lhs
$ ./quasicrystal +RTS -N
$ xview out.png
~~~~

And it looks like this:

[image]

Note that `repa-devil` silently refuses to overwrite an existing file, so you
may need to `rm out.png` first.

On my 6-core machine, this parallel code ran in 3.72 seconds of wall-clock
time, at a CPU utilization of 474%.  The same code compiled without `-threaded`
took 14.20 seconds, so the net efficiency of parallelization is 382%.  This is
a good result; what's better is how little work it required on my part.
Cutting a mere 10 seconds from a single run is not a big deal.  But it starts
to matter when rendering many frames of animation, and trying out variations on
the algorithm.

As a side note, switching from `Float` to `Double` increased the run time by
about 30%.  I suspect this is due to increased demand for memory bandwidth and
cache space.

You can grab the [Literate Haskell
source](https://github.com/kmcallister/blog-misc/blob/master/quasicrystals/quasicrystal.lhs)
and try it out on your own machine.  This is my first Repa program ever, so I'd
much appreciate feedback on improving the code.

Be sure to check out [Michael
Rule](http://wealoneonearth.blogspot.com/search/label/quasicrystal)'s work on
animating quasicrystals.
