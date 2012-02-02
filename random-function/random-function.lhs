How can we pick a random Haskell function?  Specifically, we want to write an
IO action

~~~~ {.haskell}
randomFunction :: IO (Integer -> Bool)
~~~~

with this behavior:

- It produces a function of type `Integer -> Bool`.

- It always produces a total function — a function which never throws an
  exception or enters an infinite loop.

- It is equally likely to produce *any* such function.

This is tricky, because there are infinitely many such functions (more on that
later).

In another language we might produce something which looks like a function, but
actually flips a coin on each new integer input.  It would use mutable state to
remember previous results, so that future calls will be consistent.  But the
Haskell type we gave for `randomFunction` forbids this approach.
`randomFunction` uses IO effects to pick a random function, but the function it
picks has access to neither coin flips nor mutable state.

Alternatively, we could build a lazy infinite data structure containing all the
`Bool` answers we need.  `randomFunction` could generate an [infinite list of
random `Bool`s][randomRs], and produce a function `f` which indexes into that
list.  But this indexing will be inefficient in space and time.  If the user
calls `(f 10000000)`, we'll have to run 10,000,000 steps of the pseudo-random
number generator, and build 10,000,000 list elements, before we can return a
single `Bool` result.

We can improve this considerably by using a different infinite data structure.
Though our solution is pure functional code, we *do* end up relying on mutation
— the implicit mutation by which lazy thunks become evaluated data.

[randomRs]: http://lambda.haskell.org/platform/doc/current/ghc-doc/libraries/random-1.0.0.3/System-Random.html#v:randomRs


The data structure
==================

> import System.Random
> import Data.List ( genericIndex )

Our data structure is an infinite binary tree:

> data Tree = Node Bool Tree Tree

We can interpret such a tree as a function from non-negative `Integer`s to
`Bool`s.  If the `Integer` argument is zero, the root node holds our `Bool`
answer.  Otherwise, we shift off the least-significant bit of the argument, and
look at the left or right subtree depending on that bit.

> get :: Tree -> (Integer -> Bool)
> get (Node b _ _) 0 = b
> get (Node _ x y) n =
>     case divMod n 2 of
>         (m, 0) -> get x m
>         (m, _) -> get y m

Now we need to build a suitable tree, starting from a [random number generator
state][].  The standard `System.Random` module is not going to win any [speed
contests][], but it does have one extremely nice property: it supports an
operation

~~~~ {.haskell}
split :: StdGen -> (StdGen, StdGen)
~~~~

The two generator states returned by `split` will (ideally) produce two
independent streams of random values.  We use `split` at each node of the
infinite tree.

> build :: StdGen -> Tree
> build g0 =
>     let (b,  g1) = random g0
>         (g2, g3) = split  g1
>     in  Node b (build g2) (build g3)

This is a recursive function with no base case.  Conceptually, it produces an
infinite tree.  Operationally, it produces a single `Node` constructor, whose
fields are lazily-deferred computations.  As `get` explores this notional
infinite tree, new `Node`s are created and randomness generated on demand.

`get` traverses one level per bit of its input integer.  So looking up the
integer *n* involves traversing and possibly creating <span style="white-space:
nowrap;">*O*(log *n*)</span> nodes.  This suggests good space and time
efficiency, though only testing will say for sure.

Now we have all the pieces to solve the original puzzle.  We build two trees,
one to handle positive numbers and another for negative numbers.

> randomFunction :: IO (Integer -> Bool)
> randomFunction = do
>     neg <- build `fmap` newStdGen
>     pos <- build `fmap` newStdGen
>     let f n | n < 0     = get neg (-n)
>             | otherwise = get pos n
>     return f

[random number generator state]: http://lambda.haskell.org/platform/doc/current/ghc-doc/libraries/random-1.0.0.3/System-Random.html#t:StdGen

[speed contests]: http://www.serpentine.com/blog/2009/09/19/a-new-pseudo-random-number-generator-for-haskell/


Testing
=======

Here's some code which helps us visualize one of these functions in the
vicinity of zero:

> test :: (Integer -> Bool) -> IO ()
> test f = putStrLn $ map (char . f) [-40..40] where
>     char False = ' '
>     char True  = '-'

Now we can test `randomFunction` in GHCi:

~~~~
λ> randomFunction >>= test
---- -   ---   -    - -   - --   - - -  -- --- -- --          - -- - - --  --- --
λ> randomFunction >>= test
-   ---- - - - -  - - -- -   -     ---  --- -- - --  -  --    - -  - - -  --   - 
λ> randomFunction >>= test
- ---  - - -  --  ---         -  --  -  -    -  -  - ---- - -  ---   -     -    -
~~~~

Each result from `randomFunction` is indeed a function: it always gives the
same output for a given input.  This much should be clear from the fact that we
haven't used any [unsafe shenanigans][].  But we can also demonstrate it
empirically:

~~~~
λ> f <- randomFunction
λ> test f
-   -----  - -   -- - -   --- --  - -   - -   - -   -- - -   ---- - - - -  - --- 
λ> test f
-   -----  - -   -- - -   --- --  - -   - -   - -   -- - -   ---- - - - -  - --- 
~~~~

Let's also test the speed on some very large arguments:

~~~~
λ> :set +s
λ> f 10000000
True
(0.03 secs, 12648232 bytes)
λ> f (2^65536)
True
(1.10 secs, 569231584 bytes)
λ> f (2^65536)
True
(0.26 secs, 426068040 bytes)
~~~~

The second call with `2^65536` is faster because the tree nodes already exist
in memory.  We can expect our tests to be faster yet if we compile with `ghc
-O` rather than using GHCi's bytecode interpreter.

[unsafe shenanigans]: http://lambda.haskell.org/platform/doc/current/ghc-doc/libraries/base-4.3.1.0/System-IO-Unsafe.html


How many functions?
===================

Assume we have infinite memory, so that `Integer`s really can be unboundedly
large.  And let's ignore negative numbers, for simplicity.  How many total
functions of type `Integer -> Bool` are there?

Suppose we made an infinite list `xs` of all such functions.  Now consider this
definition:

> diag :: [Integer -> Bool] -> (Integer -> Bool)
> diag xs n = not $ genericIndex xs n n

For an argument `n`, `diag xs` looks at what the `n`th function of `xs` would
return, and returns the opposite.  This means the function `diag xs` differs
from every function in our supposedly comprehensive list of functions.  This
contradiction shows that there are [uncountably many][] total functions of type
`Integer -> Bool`.  It's closely related to [Cantor's diagonal argument][] that
the real numbers are uncountable.

But wait, there are only countably many Haskell programs!  In fact, you can
encode each one as a number.  There may be uncountably many functions, but
there are only a countable number of *computable* functions.  So the proof
breaks down if you restrict it to a real programming language like Haskell.

In that context, the existence of `xs` implies that there is some *algorithm*
to enumerate the computable total functions.  This is the assumption we
ultimately contradict.  The set of computable total functions is not
[recursively enumerable][], even though it is countable.  Intuitively, to
produce a single element of this set, we would have to verify that the function
halts on every input, which is [impossible in the general case][halting
problem].

Now let's revisit `randomFunction`.  Any function it produces is computable:
the algorithm is a combination of the pseudo-random number procedure and our
tree traversal.  In this sense, `randomFunction` provides extremely poor
randomness; it only selects values from a particular [measure zero][] subset of
its result type!  But if you read the type constructor `(->)` as "computable
function", as one should in a programming language, then `randomFunction` is
closer to doing what it says it does.

[uncountably many]: http://en.wikipedia.org/wiki/Uncountable_set

[Cantor's diagonal argument]: http://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument

[recursively enumerable]: http://en.wikipedia.org/wiki/Recursively_enumerable_language

[halting problem]: http://en.wikipedia.org/wiki/Halting_problem

[measure zero]: http://en.wikipedia.org/wiki/Null_set


See also
========

The libraries [data-memocombinators][] and [MemoTrie][] use similar structures,
not for building random functions but for [memoizing][] existing ones.

You can download this post as a [Literate Haskell file][] and play with the
code.

[data-memocombinators]: http://hackage.haskell.org/package/data-memocombinators

[MemoTrie]: http://hackage.haskell.org/package/MemoTrie

[memoizing]: http://en.wikipedia.org/wiki/Memoization

[Literate Haskell file]: https://github.com/kmcallister/blog-misc/blob/master/random-function/random-function.lhs
