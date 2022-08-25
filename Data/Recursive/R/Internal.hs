{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | TODO: More documentation.
--
-- The result of 'getR' is always a solution of the given equations, but for it
-- to be deterministic (and hence for this API to be safe), the following
-- should hold:
--
-- * The @a@ in @R a@ indicates a partial order on the values of @Val a@.
-- * That partial order must respect equality on @a@
-- * It must have a bottom element 'bottom'.
-- * The function passed to 'mapR', 'liftR2' etc. must be a monotonic function between these partial orders.
--
-- If this does not hold, then the result of 'getR' may not be deterministic.
--
-- Termination depends on whether a soluiton can be found iteratively. This is
-- guaranteed if all partial orders involved satisfy the Ascending Chain Condition.
--
-- The type class 'Order' has  only 'bottom' as its member, as we do not need
-- the other operations of the order at runtime. Nevertheless such an order
-- better exists for the safety of this API, as explained in the module header.

module Data.Recursive.R.Internal
    ( R
    , Order(..)
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

data R a = R (Prop (Val a)) Thunk

-- | An instance @Order a@ indicates that @a@ names a partial order on the type @Val a@. Since a type may have multiple different useful orders (e.g. 'CanBe' and 'MustBe' for 'Bool') we use this pattern to distinguish them.
class Eq (Val a) => Order a where
    type Val a
    bottom :: Val a

r :: Val a -> R a
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
