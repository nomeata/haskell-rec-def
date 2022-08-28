{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Recursive.Propagator.Class where

import Data.Monoid (Dual)
import qualified Data.Set as S

import qualified Data.Recursive.Propagator.Naive as Naive
import Data.POrder

-- | The Propagator class defines some function shared by different propagator
-- implementations. This backs the generic "Data.Recursive.R.Internal" wrapper.
class Propagator p where
    -- | The type of values inside the propagator
    type PropVal p
    newProp :: IO p
    newConstProp :: PropVal p -> IO p
    readProp :: p -> IO (PropVal p)

instance Bottom x => Propagator (Naive.Prop x) where
    type PropVal (Naive.Prop x) = x
    newProp = Naive.newProp bottom
    newConstProp = Naive.newProp
    readProp = Naive.readProp

-- | The HasPropagator class is used to pick a propagator implementation for a
-- particular value type.
class (Propagator (Prop x), PropVal (Prop x) ~ x) => HasPropagator x where
    type Prop x

instance HasPropagator Bool where
    type Prop Bool = Naive.Prop Bool

instance HasPropagator (Dual Bool) where
    type Prop (Dual Bool) = Naive.Prop (Dual Bool)

instance Eq a => HasPropagator (S.Set a) where
    type Prop (S.Set a) = Naive.Prop (S.Set a)
