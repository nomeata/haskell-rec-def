{-|

This file contains a few examples of using this library.

Imagine you are trying to calculate a boolean value, but your calculation is
happens to be recursive. Just writing down the equations does not work:

>>> withTimeout $ let x = y || False; y = x && False in x
*** Exception: timed out

This is unfortunate, isn’t it?

This library provides data types where this works. You can write the equations
in that way just fine, and still get a result.

For example, the 'R MustBe' type comes with functions that look quite like their ordinary counterparts acting on 'Bool'.

>>> :t rTrue
rTrue :: R CanBe
>>> :t rFalse
rFalse :: R CanBe
>>> :t (|||)
(|||) :: R CanBe -> R CanBe -> R CanBe
>>> :t (&&&)
(&&&) :: R CanBe -> R CanBe -> R CanBe
>>> getR rTrue
True
>>> getR rFalse
False
>>> getR (rFalse &&& rTrue)
False
>>> getR (rTrue &&& rTrue)
True
>>> getR (rand [rTrue,  rFalse, rTrue])
False

So far so good, lets see what happens when we try something recursive:

>>> let x = rand [y]; y = rand [x, rFalse] in getR x
False
>>> let x = rand [y]; y = rand [x, rTrue] in getR x
True
>>> let x = rand [y]; y = rand [x] in getR x
True

The last equation is interesting: We essentially say that x is True if y is
True, and y is True if x is True. This has two solutions, we can either set
both to 'True' and both to 'False'.

This is the reason why the type is 'R CanBe', and not just 'R Bool': It finds out whether the value _can_ be true. And abvoe, we see that indeed it can be True.

There is also the dual, 'MustBe'. Because its module exports functions of the same name, we have imported it qualified. We can run run the same quations, and get different answers:

>>> let x = MB.rand [y]; y = MB.rand [x, r False] in getR x
False
>>> let x = MB.rand [y]; y = MB.rand [x, r True] in getR x
False
>>> let x = MB.rand [y]; y = MB.rand [x] in getR x
False

The negation function is also available, and goes from can-be-true to must-be-true and back:
>>> :t rnot
rnot :: R MB.MustBe -> R CanBe
>>> :t MB.rnot
MB.rnot :: R CanBe -> R MB.MustBe

This allows us to mix the different types in the same computation:
>>> :{
  let x = rnot y &&& rnot z
      y = MB.rnot x MB.||| z
      z = MB.rTrue
  in (getR x, getR y, getR z)
 :}
(False,True,True)

>>> :{
  let x = rnot y &&& rnot z
      y = MB.rnot x MB.||| z
      z = MB.rFalse
  in (getR x, getR y, getR z)
 :}
(True,False,False)

We do not have to stop with booleans, and can define similar APIs for other
data stuctures, e.g. sets:

Again we can describe sets recursively, using the monotone functions 'pEmpty',
'pInsert' and 'pUnion'

>>> :{
  let s1 = rInsert 23 s2
      s2 = rInsert 42 s1
  in getR s1
 :}
fromList [23,42]

Here is a slightly larger example, where we can can use this API to elegantly
calculate the reachable nodes in a graph (represented as a map from vertices to
their successors), using a typical knot-tying approach. But unless with plain
'Set', it now works even if the graph has cycles:

>>> :{
   reachable :: M.Map Int [Int] -> M.Map Int (S.Set Int)
   reachable g = fmap getR sets
     where
       sets :: M.Map Int (R (S.Set Int))
       sets = M.mapWithKey (\v vs -> rInsert v (rUnions [ sets ! v' | v' <- vs ])) g
 :}

>>> let graph = M.fromList [(1,[2,3]),(2,[1]),(3,[])]
>>> reachable graph M.! 1
fromList [1,2,3]
>>> reachable graph M.! 3
fromList [3]


Of course, the magic stops somewhere: Just like with the usual knot-tying
tricks, you still have to make sure to be lazy enough. In particular, you should
not peek at the value (e.g. using 'getR') while you are building the graph:
>>> :{
    withTimeout $
      let x = rand [x, if getR y then z else rTrue]
          y = rand [x, rTrue]
          z = rFalse
      in getR y
    :}
*** Exception: timed out

Similarly, you have to make sure you recurse through one of these functions; @let x = x@ still does not work:
>>> withTimeout $ let x = x :: R CanBe in getR x
*** Exception: timed out
>>> withTimeout $ let x = x &&& x in getR x
True

-}

module Examples where
import Data.Recursive.R
import Data.Recursive.CanBe
import qualified Data.Recursive.MustBe as MB
import Data.Recursive.Set

import System.Timeout
import Control.Exception
import Data.Maybe
import Data.Map as M
import qualified Data.Set as S
import GHC.Err

withTimeout :: a -> IO a
withTimeout a =
    fromMaybe (errorWithoutStackTrace "timed out") <$>
        timeout 100000 (evaluate a)
