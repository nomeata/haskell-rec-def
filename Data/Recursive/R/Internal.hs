{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | TODO: More documentation.
--
-- The result of 'getR' is always a solution of the given equations, but for it
-- to be deterministic (and hence for this API to be safe), the following
-- should hold:
--
-- * The @a@ in @R a@ should be partially orderd ('POrder')
-- * That partial order must respect equality on @a@
-- * It must have a bottom element 'bottom' ('Bottom').
-- * The function passed to 'mapR', 'liftR2' etc. must be a monotonic function
--   between these partial orders.
--
-- If this does not hold, then the result of 'getR' may not be deterministic.
--
-- Termination depends on whether a soluiton can be found iteratively. This is
-- guaranteed if all partial orders involved satisfy the Ascending Chain Condition.

module Data.Recursive.R.Internal
    ( R
    , r, mapR, liftR2, liftRList
    , getR, getRDual
    )
where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Recursive.Propagator.Naive
import System.IO.RecThunk
import Data.POrder

data R a = R (Prop a) Thunk

r :: a -> R a
r x = unsafePerformIO $ do
    p <- newProp x
    t <- doneThunk
    pure (R p t)

newR :: Bottom a => (Prop a -> IO [KickedThunk]) -> R a
newR act = unsafePerformIO $ do
    p <- newProp bottom
    t <- thunk (act p)
    pure (R p t)

mapR :: Bottom b => (a -> b) -> R a -> R b
mapR f r1 = newR $ \p -> do
    let R p1 t1 = r1
    lift1 f p1 p
    mapM kick [t1]

liftR2 :: Bottom c => (a -> b -> c) -> R a -> R b -> R c
liftR2 f r1 r2 = newR $ \p -> do
    let R p1 t1 = r1
    let R p2 t2 = r2
    lift2 f p1 p2 p
    mapM kick [t1, t2]

liftRList :: Bottom c => ([a] -> c) -> [R a] -> R c
liftRList f rs = newR $ \p -> do
    liftList f [ p' | R p' _ <- rs] p
    mapM (\(R _ t) -> kick t) rs

getR :: R a -> a
getR (R p t) = unsafePerformIO $ do
    force t
    readProp p

getRDual :: R (Dual a) -> a
getRDual = getDual . getR
