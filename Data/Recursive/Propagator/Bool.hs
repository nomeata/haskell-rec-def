{-# LANGUAGE LambdaCase #-}

-- | A propagator API just for booleans
module Data.Recursive.Propagator.Bool where

import Control.Concurrent.MVar

-- The imperative part

data MustBeTrue
    = SurelyTrue
    | SurelyFalse       -- this is mostly an optimization
    | MaybeTrue (IO ()) -- action to run when this turns out to be true

newtype PBool = PBool (MVar MustBeTrue)

surelyTrue :: IO PBool
surelyTrue = PBool <$> newMVar SurelyTrue

surelyFalse :: IO PBool
surelyFalse = PBool <$> newMVar SurelyFalse

maybeTrue :: IO PBool
maybeTrue = PBool <$> newMVar (MaybeTrue (pure ()))

setTrue ::  PBool -> IO ()
setTrue (PBool p) = takeMVar p >>= \case
    SurelyFalse -> error "setTrue: Argument is surely False"
    SurelyTrue -> putMVar p SurelyTrue
    MaybeTrue act -> do
        -- Do this first, this breaks cycles
        putMVar p SurelyTrue
        -- Now notify the dependencies
        act

whenTrue :: PBool -> IO () -> IO ()
whenTrue (PBool p1) act = takeMVar p1 >>= \case
    SurelyTrue -> putMVar p1 SurelyTrue >> act
    SurelyFalse -> putMVar p1 SurelyFalse
    MaybeTrue act' -> putMVar p1 $ MaybeTrue (act >> act')

implies :: PBool -> PBool -> IO ()
implies p1 p2 = whenTrue p1 (setTrue p2)

mustBeTrue :: PBool -> IO Bool
mustBeTrue (PBool p) = readMVar p >>= \case
    SurelyTrue -> pure True
    _ -> pure False

newtype PDualBool = PDualBool PBool
