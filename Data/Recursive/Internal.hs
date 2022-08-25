{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: More documentation.
--
-- The result of 'getR' is always a solution of the given equations, but for it
-- to be deterministic (and hence for this API to be safe), the following
-- should hold:
--
-- * Every 'R a' has an (implicit) with a partial order on the values of 'a'
-- * That partial order must respect equality on 'a'
-- * The first argument to 'mapR x f :: R a -> R b' must be the bottom element
--   of the partial order in 'b'
-- * The second argument must a monotone function from the partial order on 'a'
--   to the partial order on 'b'
--
-- If this does not hold, then the result of 'getR' may not be deterministic.
--
-- Termination depends on whether a soluiton can be found iteratively. This is
-- guaranteed if all partial orders involved satisfy the Ascending Chain Condition.

module Data.Recursive.Internal
    ( R
    , r, mapR, liftR2, liftRList
    , getR
    )
where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Recursive.Propagator
import Data.Recursive.Thunk
import Data.Recursive.Class

data R a = R (Prop (Val a)) Thunk

r :: Order a => Val a -> R a
r x = unsafePerformIO $ do
    p <- newProp x
    t <- doneThunk
    pure (R p t)

newR :: forall a. Order a => (Prop (Val a) -> IO [KickedThunk]) -> R a
newR act = unsafePerformIO $ do
    p <- newProp (bottom @a)
    t <- thunk (act p)
    pure (R p t)

mapR :: Order b => (Val a -> Val b) -> R a -> R b
mapR f r1 = newR $ \p -> do
    let R p1 t1 = r1
    lift1 f p1 p
    mapM kick [t1]

liftR2 :: Order c => (Val a -> Val b -> Val c) -> R a -> R b -> R c
liftR2 f r1 r2 = newR $ \p -> do
    let R p1 t1 = r1
    let R p2 t2 = r2
    lift2 f p1 p2 p
    mapM kick [t1, t2]

liftRList :: Order c => ([Val a] -> Val c) -> [R a] -> R c
liftRList f rs = newR $ \p -> do
    liftList f [ p' | R p' _ <- rs] p
    mapM (\(R _ t) -> kick t) rs

getR :: R a -> Val a
getR (R p t) = unsafePerformIO $ do
    force t
    readProp p
