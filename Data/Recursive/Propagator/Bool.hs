{-# LANGUAGE LambdaCase #-}

-- | A propagator API just for booleans
module Data.Recursive.Propagator.Bool where

import Control.Concurrent.MVar

-- The imperative part

data MustBeTrue
    = SurelyTrue
    | SurelyFalse -- this is mostly an optimization
    | MaybeTrue [PBool] -- stores pointers to dependent variables

newtype PBool = PBool (MVar MustBeTrue)

surelyTrue :: IO PBool
surelyTrue = PBool <$> newMVar SurelyTrue

surelyFalse :: IO PBool
surelyFalse = PBool <$> newMVar SurelyFalse

maybeTrue :: IO PBool
maybeTrue = PBool <$> newMVar (MaybeTrue [])

setTrue ::  PBool -> IO ()
setTrue (PBool p) = takeMVar p >>= \case
    SurelyFalse -> error "setTrue: Argument is surely False"
    SurelyTrue -> putMVar p SurelyTrue
    MaybeTrue ls -> do
        -- Do this first, this breaks cycles
        putMVar p SurelyTrue
        -- Now notify the dependencies
        mapM_ setTrue ls

implies :: PBool -> PBool -> IO ()
implies (PBool p1) p2 = takeMVar p1 >>= \case
    SurelyTrue -> putMVar p1 SurelyTrue >> setTrue p2
    SurelyFalse -> putMVar p1 SurelyFalse
    MaybeTrue ps -> putMVar p1 $ MaybeTrue (p2 : ps)

mustBeTrue :: PBool -> IO Bool
mustBeTrue (PBool p) = readMVar p >>= \case
    SurelyTrue -> pure True
    _ -> pure False

