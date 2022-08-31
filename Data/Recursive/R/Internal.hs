{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- This module provides the 'R' data type, which wraps an imperative propagator (e.g. "Data.Recursive.Propagator.Naive") in a pure and (if done right) safe data structure.
--
-- The result of 'getR' is always a solution of the given equations, but for it
-- to be deterministic (and hence for this API to be safe), the following
-- should hold:
--
-- * The @a@ in @R a@ should be partially orderd ('Data.POrder.POrder')
-- * That partial order must respect equality on @a@
-- * It must have a bottom element 'Data.POrder.bottom' ('Data.POrder.Bottom').
-- * The function passed to 'defR1', 'defR2' etc. must be a monotonic function
--   between these partial orders.
--
-- If this does not hold, then the result of 'getR' may not be deterministic.
--
-- Termination depends on whether a soluiton can be found iteratively. This is
-- guaranteed if all partial orders involved satisfy the Ascending Chain Condition.

module Data.Recursive.R.Internal
    ( R
    , getR, getRDual
    , mkR, defR1, defR2, defRList
    )
where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Recursive.Propagator.Class
import System.IO.RecThunk

-- | A value of type @R a@ is a @a@, but defined using only specific operations
-- (which you will find in the corresponding module, e.g.
-- "Data.Recursive.Bool"), which allow recursive definitions.
--
-- You can use 'getR' to extract the value.
--
-- Do not use the extracted value in the definition of that value, this will
-- loop just like a recursive definition with plain values would.
data R a = R (Prop a) Thunk

-- | Any value of type @a@ is also a value of type @r a@.
mkR :: HasPropagator a => a -> R a
mkR x = unsafePerformIO $ do
    p <- newConstProp x
    t <- doneThunk
    pure (R p t)

newR :: HasPropagator a => (Prop a -> IO [Thunk]) -> R a
newR act = unsafePerformIO $ do
    p <- newProp
    t <- thunk (act p)
    pure (R p t)

-- | Defines a value of type @R b@ to be a function of the values of @R a@.
--
-- The action passed it should declare that relation to the underlying propagator.
--
-- The @Prop a@ propagator must only be used for reading values _from_.
defR1 :: (HasPropagator a, HasPropagator b) =>
    (Prop a -> Prop b -> IO ()) ->
    R a -> R b
defR1 def r1 = newR $ \p -> do
    let R p1 t1 = r1
    def p1 p
    pure [t1]

-- | Defines a value of type @R c@ to be a function of the values of @R a@ and @R b@.
--
-- The action passed it should declare that relation to the underlying propagator.
--
-- The @Prop a@ and @Prop b@ propagators must only be used for reading values _from_.
defR2 :: (HasPropagator a, HasPropagator b, HasPropagator c) =>
    (Prop a -> Prop b -> Prop c -> IO ()) ->
    R a -> R b -> R c
defR2 def r1 r2 = newR $ \p -> do
    let R p1 t1 = r1
    let R p2 t2 = r2
    def p1 p2 p
    pure [t1, t2]

-- | Defines a value of type @R b@ to be a function of the values of a list of @R a@ values.
--
-- The action passed it should declare that relation to the underlying propagator.
--
-- The @Prop a@ propagators must only be used for reading values _from_.
defRList :: (HasPropagator a, HasPropagator b) =>
    ([Prop a] -> Prop b -> IO ()) ->
    [R a] -> R b
defRList def rs = newR $ \p -> do
    def [ p' | R p' _ <- rs] p
    pure [ t | R _ t <- rs]

-- | Extract the value from a @R a@. This must not be used when _defining_ that value.
getR :: HasPropagator a => R a -> a
getR (R p t) = unsafePerformIO $ do
    force t
    readProp p

-- | Convenience variant of 'getR' to also remove the 'Dual' newtype wrapper, mostly for use with "Data.Recursive.DualBool".
getRDual :: HasPropagator (Dual a) => R (Dual a) -> a
getRDual = getDual . getR
