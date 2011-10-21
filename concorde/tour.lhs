The [Traveling Salesperson
Problem](http://en.wikipedia.org/wiki/Travelling_salesman_problem) (TSP) is a
famous optimization problem with applications in logistics, manufacturing, and
[art](http://www.cgl.uwaterloo.ca/~csk/projects/tsp/).  In its [planar
form](http://en.wikipedia.org/wiki/Euclidean_traveling_salesman_problem#Euclidean_TSP),
we are given a set of "cities", and we want to visit each city while minimizing
the total travel distance.

Finding the shortest possible tour is
[NP-hard](http://en.wikipedia.org/wiki/NP-hard), and quickly becomes infeasible
as the number of cities grows.  But most applications need only a heuristically
good solution: a tour which is short, if not the shortest possible.  The
[Lin-Kernighan
heuristic](http://en.wikipedia.org/wiki/Lin%E2%80%93Kernighan_heuristic)
quickly produces such tours.

The [Concorde](http://www.tsp.gatech.edu/concorde.html) project provides a
[well-regarded](http://www.jstatsoft.org/v23/i02) collection of TSP solvers.  I
needed TSP heuristics for a Haskell project, so I wrote a [Haskell
interface](http://hackage.haskell.org/package/concorde) to Concorde's
Lin-Kernighan implementation.  Concorde provides a [C
library](http://www.tsp.gatech.edu/concorde/DOC/concorde_org.html), but it's
far from clear how to use it.  Instead I chose to invoke the `linkern`
executable as a subprocess.

The core of the Haskell interface looks like this:

~~~~ {.haskell}
tsp
  :: Config     -- provides various configurable parameters
  -> (a -> R2)  -- gives the rectangular coordinates of each point
  -> [a]        -- list of points to visit
  -> IO [a]     -- produces points permuted in tour order
~~~~

`tsp` lets you represent the points to visit using any type you like.  You just
provide a function to get the coordinates of each point.  The `Config`
parameter controls various aspects of the computation, including the
time/quality tradeoff.  Defaults are provided, and you can override these
selectively using record-update syntax.  All considered it's a pretty simple
interface which tries to hide the complexity of interacting with an external
program.


Visualizing a tour
==================

Here's a example program which computes a tour of 1,000 random points.  We'll
visualize the tour using the [Diagrams](http://projects.haskell.org/diagrams/)
library.

> import Diagrams.Prelude
>     ( Diagram  , Point(P), fillColor   , lineWidth
>     , translate, circle  , fromVertices, lightgrey )
>
> import Diagrams.Backend.Cairo.CmdLine
>     ( Cairo, defaultMain )
>
> import Data.Colour.SRGB         ( sRGB       )
> import Data.Colour.RGBSpace     ( uncurryRGB )
> import Data.Colour.RGBSpace.HSV ( hsv        )
>
> import qualified Algorithms.Concorde.LinKern as T
>
> import Control.Monad
> import Data.Monoid
> import System.Random

`tsp` takes a list of points and a function to extract the coordinates of a
point.  Our points are just the coordinates themselves, so we pass the identity
function.

> type R2 = (Double, Double)
>
> findTour :: [R2] -> IO [R2]
> findTour = T.tsp cfg id where
>     cfg = T.defConfig { T.verbose = True }

The tour is drawn as a loop of line segements.  We also shade the interior of
this polygon.

> diaTour :: [R2] -> Diagram Cairo R2
> diaTour xs@(x:_) = sty . fromVertices $ map P (xs ++ [x]) where
>     sty = fillColor lightgrey . lineWidth 10

Each point visited by the tour is drawn as a circle, with hue indicating its
position in the tour.

> diaPoints :: [R2] -> Diagram Cairo R2
> diaPoints = mconcat . map circ . zip [0..] where
>     n = fromIntegral numPoints
>     circ (i,p) = translate p . fillColor color $ circle 40
>         where color = uncurryRGB sRGB (hsv (360*i/n) 1 1)

Now we put it all together.  Note that `linkern` uses Euclidean distances
rounded to the nearest integer.  So we need coordinates with fairly large
magnitudes.  Picking values between 0 and 1 won't work.

> numPoints :: Int
> numPoints = 1000
>
> main :: IO ()
> main = do
>     let rnd = randomRIO (0,10000)
>     points <- replicateM numPoints (liftM2 (,) rnd rnd)
>     tour   <- findTour points
>     defaultMain (diaPoints tour `mappend` diaTour tour)

We run it like so:

~~~~
$ export PATH=~/concorde-031219/LINKERN:$PATH
$ runhaskell tour.lhs -o out.pdf
$ xpdf out.pdf
~~~~

The computation takes about 2 seconds on my machine.  And the output looks like
this:

[image]

You can download this post as a [Literate Haskell
file](https://github.com/kmcallister/blog-misc/blob/master/concorde/tour.lhs)
and run the above program.  You'll need to install the
[`concorde`](http://hackage.haskell.org/package/concorde) and
[`diagrams`](http://hackage.haskell.org/package/diagrams) packages.

The [source](https://github.com/kmcallister/concorde) for the `concorde`
Haskell package includes a [more full-featured
version](https://github.com/kmcallister/concorde/blob/master/examples/visualize.hs)
of this example.
