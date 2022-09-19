{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module provides the 'Propagator' class
module Data.Propagator.Class where

-- | The Propagator class defines some functions shared by different propagator
-- implementations. This backs the generic "Data.Propagator.Purify" wrapper.
class Propagator p x | p -> x where
    newProp :: IO p
    newConstProp :: x -> IO p
    readProp :: p -> IO x
