{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|

This file contains a few examples of using the @rec-def@ library. There is no
need to actually use this module.

= A @rec-def@ tutorial

Imagine you are trying to calculate a boolean value, but your calculation is
happens to be recursive. Just writing down the equations does not work:

>>> withTimeout $ let x = y || False; y = x && False in x
*** Exception: timed out

This is unfortunate, isn’t it?

== A @Bool@ with recursive equations

This library provides data types where this works. You can write the equations
in that way just fine, and still get a result.

For example, the 'Data.Recursive.Bool.RBool' type comes with functions that look quite like their
ordinary counterparts acting on 'Bool'.

>>> import Data.Recursive.Bool (RBool)
>>> import qualified Data.Recursive.Bool as RB

>>> :t RB.true
RB.true :: RBool
>>> :t RB.false
RB.false :: RBool
>>> :t (RB.||)
(RB.||) :: RBool -> RBool -> RBool
>>> :t (RB.&&)
(RB.&&) :: RBool -> RBool -> RBool
>>> RB.get RB.true
True
>>> RB.get RB.false
False
>>> RB.get (RB.false RB.&& RB.true)
False
>>> RB.get (RB.true RB.&& RB.true)
True
>>> RB.get (RB.or [RB.true,  RB.false, RB.true])
True

So far so good, lets see what happens when we try something recursive:

>>> let x = RB.or [y]; y = RB.and [x, RB.false] in RB.get x
False
>>> let x = RB.or [y]; y = RB.or [x, RB.false] in RB.get x
False
>>> let x = RB.or [y]; y = RB.or [x, RB.true] in RB.get x
True
>>> let x = RB.or [y]; y = RB.or [x] in RB.get x
False

== Least or greatest solution

The last equation is interesting: We essentially say that @x@ is @True@ if @y@ is
@True@, and @y@ is @True@ if @x@ is @True@. This has two solutions, we can either set
both to @True@ and both to @False@.

We (arbitrary) choose to find the least solution, i.e. prefer @False@ and
only find @True@ if we have to. This is useful, for example, if you check something recursive for errors.

Sometimes you want the other one. Then you can use @RDualBool@. The module
"Data.Recursive.DualBool" exports all the functions for that type too. We can
run the same equations, and get different answers:

>>> import Data.Recursive.DualBool (RDualBool)
>>> import qualified Data.Recursive.DualBool as RDB


>>> let x = RDB.or [y]; y = RDB.and [x, RDB.false] in RDB.get x
False
>>> let x = RDB.or [y]; y = RDB.or [x, RDB.false] in RDB.get x
True
>>> let x = RDB.or [y]; y = RDB.or [x, RDB.true] in RDB.get x
True
>>> let x = RDB.or [y]; y = RDB.or [x] in RDB.get x
True

The negation function is also available, and goes from can-be-true to must-be-true and back:

>>> :t RB.not
RB.not :: RDualBool -> RBool
>>> :t RDB.not
RDB.not :: RBool -> RDualBool

This allows us to mix the different types in the same computation:

>>> :{
  let x = RB.not y RB.|| RB.not z
      y = RDB.not x RDB.&& z
      z = RDB.true
  in (RB.get x, RDB.get y, RDB.get z)
 :}
(False,True,True)

>>> :{
  let x = RB.not y RB.|| RB.not z
      y = RDB.not x RDB.&& z
      z = RDB.false
  in (RB.get x, RDB.get y, RDB.get z)
 :}
(True,False,False)

== Sets

We do not have to stop with booleans, and can define similar APIs for other
data stuctures, e.g. sets:

>>> import qualified Data.Recursive.Set as RS

Again we can describe sets recursively, using the monotone functions 'RS.empty',
'RS.insert' and 'RS.union'

>>> :{
  let s1 = RS.insert 23 s2
      s2 = RS.insert 42 s1
  in RS.get s1
 :}
fromList [23,42]

Here is a slightly larger example, where we can use this API to elegantly
calculate the reachable nodes in a graph (represented as a map from vertices to
their successors), using a typical knot-tying approach. But unless with plain
'S.Set', it now works even if the graph has cycles:

>>> :{
   reachable :: M.Map Int [Int] -> M.Map Int (S.Set Int)
   reachable g = fmap RS.get sets
     where
       sets :: M.Map Int (RS.RSet Int)
       sets = M.mapWithKey (\v vs -> RS.insert v (RS.unions [ sets ! v' | v' <- vs ])) g
 :}

>>> let graph = M.fromList [(1,[2,3]),(2,[1]),(3,[])]
>>> reachable graph M.! 1
fromList [1,2,3]
>>> reachable graph M.! 3
fromList [3]

== Caveats

Of course, the magic stops somewhere: Just like with the usual knot-tying
tricks, you still have to make sure to be lazy enough. In particular, you should
not peek at the value (e.g. using 'RB.get') while you are building the graph:

>>> :{
    withTimeout $
      let x = RB.and [x, if RB.get y then z else RB.true]
          y = RB.and [x, RB.true]
          z = RB.false
      in RB.get y
    :}
*** Exception: timed out

Similarly, you have to make sure you recurse through one of these functions; @let x = x@ still does not work:

>>> withTimeout $ let x = x :: RBool in RB.get x
*** Exception: timed out
>>> withTimeout $ let x = x RB.&& x in RB.get x
False

We belive that the APIs provided here are still “pure”: evaluation order does not affect the results, and you can replace equals with equals, in the sense that

> let s = RS.insert 42 s in s

is the same as

> let s = RS.insert 42 s in RS.insert 42 s

However, the the following two expressions are not equivalent:

>>> withTimeout $ S.toList $ let s = RS.insert 42 s in RS.get s
[42]
>>> withTimeout $ S.toList $ let s () = RS.insert 42 (s ()) in RS.get (s ())
*** Exception: timed out

It is debatable if that is a problem.

-}
module Data.Recursive.Examples () where

-- Imports for haddock

import qualified Data.Recursive.Bool as RB
import qualified Data.Recursive.DualBool as RDB
import qualified Data.Recursive.Set as RS

-- $setup
--
-- >>> import System.Timeout
-- >>> import Control.Exception
-- >>> import Data.Maybe
-- >>> import Data.Map as M
-- >>> import qualified Data.Set as S
-- >>>
-- >>> :{
-- let withTimeout :: Show a => a -> IO a
--     withTimeout a =
--       fromMaybe (errorWithoutStackTrace "timed out") <$>
--          timeout 100000 (length (show a) `seq` evaluate a)
-- :}


