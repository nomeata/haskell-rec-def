{-|

This file contains a few examples of using the @rec-def@ library. There is no need to actually use this module.

Imagine you are trying to calculate a boolean value, but your calculation is
happens to be recursive. Just writing down the equations does not work:

>>> withTimeout $ let x = y || False; y = x && False in x
*** Exception: timed out

This is unfortunate, isn’t it?

This library provides data types where this works. You can write the equations
in that way just fine, and still get a result.

For example, the 'R Bool' type comes with functions that look quite like their
ordinary counterparts acting on 'Bool'.

>>> :t rTrue
rTrue :: R Bool
>>> :t rFalse
rFalse :: R Bool
>>> :t (|||)
(|||) :: R Bool -> R Bool -> R Bool
>>> :t (&&&)
(&&&) :: R Bool -> R Bool -> R Bool
>>> getR rTrue
True
>>> getR rFalse
False
>>> getR (rFalse &&& rTrue)
False
>>> getR (rTrue &&& rTrue)
True
>>> getR (ror [rTrue,  rFalse, rTrue])
True

So far so good, lets see what happens when we try something recursive:

>>> let x = ror [y]; y = rand [x, rFalse] in getR x
False
>>> let x = ror [y]; y = ror [x, rFalse] in getR x
False
>>> let x = ror [y]; y = ror [x, rTrue] in getR x
True
>>> let x = ror [y]; y = ror [x] in getR x
False

The last equation is interesting: We essentially say that @x@ is @True@ if @y@ is
@True@, and @y@ is @True@ if @x@ is @True@. This has two solutions, we can either set
both to @True@ and both to @False@.

We (arbitrary) choose to find the smallest solution, i.e. prefer @False@ and
only find @True@ if we have to. This is useful, for example, if you check something recursive for errors.

Sometimes you want the other one. Then you can use @R (Dual Bool)@. The module
"Data.Recursive.DualBool" exports all the functions for that type too. Because
of the name class we have imported it qualified here. We can run run the same
quations, and get different answers:

>>> let x = DB.ror [y]; y = DB.rand [x, DB.rFalse] in getRDual x
False
>>> let x = DB.ror [y]; y = DB.ror [x, DB.rFalse] in getRDual x
True
>>> let x = DB.ror [y]; y = DB.ror [x, DB.rTrue] in getRDual x
True
>>> let x = DB.ror [y]; y = DB.ror [x] in getRDual x
True

The negation function is also available, and goes from can-be-true to must-be-true and back:

>>> :t rnot
rnot :: R (Dual Bool) -> R Bool
>>> :t DB.rnot
DB.rnot :: R Bool -> R (Dual Bool)

This allows us to mix the different types in the same computation:

>>> :{
  let x = rnot y ||| rnot z
      y = DB.rnot x DB.&&& z
      z = DB.rTrue
  in (getR x, getRDual y, getRDual z)
 :}
(False,True,True)

>>> :{
  let x = rnot y ||| rnot z
      y = DB.rnot x DB.&&& z
      z = DB.rFalse
  in (getR x, getRDual y, getRDual z)
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

>>> withTimeout $ let x = x :: R Bool in getR x
*** Exception: timed out
>>> withTimeout $ let x = x &&& x in getR x
False

-}
module Data.Recursive.Examples () where

import Data.Recursive.R
import Data.Recursive.Bool
import qualified Data.Recursive.DualBool as DB
import Data.Recursive.Set
import Data.Monoid

import System.Timeout
import Control.Exception
import Data.Maybe
import Data.Map as M
import qualified Data.Set as S

withTimeout :: a -> IO a
withTimeout a =
    fromMaybe (errorWithoutStackTrace "timed out") <$>
        timeout 100000 (evaluate a)
