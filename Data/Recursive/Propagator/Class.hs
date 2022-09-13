{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module provides the 'Propagator' and 'HasPropagator' classes.
module Data.Recursive.Propagator.Class where

import Data.Monoid (Dual(..))
import qualified Data.Set as S
import Data.Coerce

import qualified Data.Recursive.Propagator.Naive as Naive
import Data.Recursive.Propagator.P2
import Data.POrder

-- | The Propagator class defines some functions shared by different propagator
-- implementations. This backs the generic "Data.Recursive.R.Internal" wrapper.
class Propagator p x | p -> x where
    -- | The type of values inside the propagator
    newProp :: IO p
    newConstProp :: x -> IO p
    readProp :: p -> IO x

instance Bottom x => Propagator (Naive.Prop x) x where
    newProp = Naive.newProp
    newConstProp = Naive.newConstProp
    readProp = Naive.readProp

instance Propagator PBool Bool where
    newProp = coerce newP2
    newConstProp False = coerce newP2
    newConstProp True = coerce newTopP2
    readProp = coerce isTop

instance Propagator PDualBool (Dual Bool) where
    newProp = coerce newP2
    newConstProp (Dual True) = coerce newP2
    newConstProp (Dual False) = coerce newTopP2
    readProp = coerce $ fmap not . isTop

-- | The HasPropagator class is used to pick a propagator implementation for a
-- particular value type.
class Propagator (Prop x) x => HasPropagator x where
    type Prop x

instance HasPropagator Bool where
    type Prop Bool = PBool

instance HasPropagator (Dual Bool) where
    type Prop (Dual Bool) = PDualBool

instance Eq a => HasPropagator (S.Set a) where
    type Prop (S.Set a) = Naive.Prop (S.Set a)
