{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Recursive.Propagator.Class where

import Data.Monoid (Dual)
import qualified Data.Set as S

import qualified Data.Recursive.Propagator.Naive as Naive
import qualified Data.Recursive.Propagator.Bool as PBool
import Data.POrder

-- | The Propagator class defines some function shared by different propagator
-- implementations. This backs the generic "Data.Recursive.R.Internal" wrapper.
class Propagator p x | p -> x where
    -- | The type of values inside the propagator
    newProp :: IO p
    newConstProp :: x -> IO p
    readProp :: p -> IO x

instance Bottom x => Propagator (Naive.Prop x) x where
    newProp = Naive.newProp bottom
    newConstProp = Naive.newProp
    readProp = Naive.readProp

instance Propagator PBool.PBool Bool where
    newProp = PBool.maybeTrue
    newConstProp True = PBool.surelyTrue
    newConstProp False = PBool.surelyFalse
    readProp = PBool.mustBeTrue

-- | The HasPropagator class is used to pick a propagator implementation for a
-- particular value type.
class Propagator (Prop x) x => HasPropagator x where
    type Prop x

instance HasPropagator Bool where
    type Prop Bool = Naive.Prop Bool

instance HasPropagator (Dual Bool) where
    type Prop (Dual Bool) = Naive.Prop (Dual Bool)

instance Eq a => HasPropagator (S.Set a) where
    type Prop (S.Set a) = Naive.Prop (S.Set a)
