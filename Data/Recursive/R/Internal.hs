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
    , r, defR1, defR2, defRList
    , getR, getRDual
    )
where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Recursive.Propagator.Class
import System.IO.RecThunk

data R a = R (Prop a) Thunk

r :: HasPropagator a => a -> R a
r x = unsafePerformIO $ do
    p <- newConstProp x
    t <- doneThunk
    pure (R p t)

newR :: HasPropagator a => (Prop a -> IO [KickedThunk]) -> R a
newR act = unsafePerformIO $ do
    p <- newProp
    t <- thunk (act p)
    pure (R p t)

defR1 :: (HasPropagator a, HasPropagator b) =>
    (Prop a -> Prop b -> IO ()) ->
    R a -> R b
defR1 def r1 = newR $ \p -> do
    let R p1 t1 = r1
    def p1 p
    mapM kick [t1]

defR2 :: (HasPropagator a, HasPropagator b, HasPropagator c) =>
    (Prop a -> Prop b -> Prop c -> IO ()) ->
    R a -> R b -> R c
defR2 def r1 r2 = newR $ \p -> do
    let R p1 t1 = r1
    let R p2 t2 = r2
    def p1 p2 p
    mapM kick [t1, t2]

defRList :: (HasPropagator a, HasPropagator b) =>
    ([Prop a] -> Prop b -> IO ()) ->
    [R a] -> R b
defRList def rs = newR $ \p -> do
    def [ p' | R p' _ <- rs] p
    mapM (\(R _ t) -> kick t) rs

getR :: HasPropagator a => R a -> a
getR (R p t) = unsafePerformIO $ do
    force t
    readProp p

getRDual :: HasPropagator (Dual a) => R (Dual a) -> a
getRDual = getDual . getR
