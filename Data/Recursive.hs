{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

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

module Data.Recursive
    ( R
    , pureR, mapR, liftR2, liftRList
    , getR
    )
where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Recursive.Propagator
import Data.Recursive.Thunk

data R a = R (Prop a) Thunk

-- Almost an applicative, but we have to pass in the default (bottom) element

pureR :: a -> R a
pureR x = unsafePerformIO $ do
    p <- newProp x
    t <- doneThunk
    pure (R p t)

mapR :: Eq b => b -> (a -> b) -> R a -> R b
mapR bottom f r = unsafePerformIO $ do
    p <- newProp bottom
    t <- thunk $ do
        -- NB: Only peek at r inside thunk!
        let R p1 t1 = r
        lift1 f p1 p
        mapM kick [t1]
    pure (R p t)

liftR2 :: Eq c => c -> (a -> b -> c) -> R a -> R b -> R c
liftR2 bottom f r1 r2 = unsafePerformIO $ do
    p <- newProp bottom
    t <- thunk $ do
        let R p1 t1 = r1
        let R p2 t2 = r2
        lift2 f p1 p2 p
        mapM kick [t1, t2]
    pure (R p t)

-- also a n-ary version?
liftRList :: Eq c => c -> ([a] -> c) -> [R a] -> R c
liftRList bottom f rs = unsafePerformIO $ do
    p <- newProp bottom
    t <- thunk $ do
        liftList f [ p | R p _ <- rs] p
        mapM (\(R _ t) -> kick t) rs
    pure (R p t)

getR :: R a -> a
getR (R p t) = unsafePerformIO $ do
    force t
    readProp p
