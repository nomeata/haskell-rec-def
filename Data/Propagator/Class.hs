{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module provides the 'Propagator' class
module Data.Propagator.Class where

import Control.Exception

-- | The Propagator class defines some functions shared by different propagator
-- implementations. This backs the generic "Data.Propagator.Purify" wrapper.
class Propagator p x | p -> x where
    newProp :: IO p
    newConstProp :: x -> IO p
    freezeProp :: p -> IO ()
    readProp :: p -> IO x

-- | Exception thrown by a propagator when attempting to change the value of a frozen propagator
data WriteToFrozenPropagatorException = WriteToFrozenPropagatorException
   deriving Show
instance Exception WriteToFrozenPropagatorException
