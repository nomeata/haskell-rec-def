{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- | TODO: More documentation.
--
-- In particular, describe when this is safe to use.
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
