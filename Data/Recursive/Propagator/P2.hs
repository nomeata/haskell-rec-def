{-# LANGUAGE LambdaCase #-}

-- | A propagator for the two-point lattice
--
module Data.Recursive.Propagator.P2
    ( P2
    , newP2
    , newTopP2
    , setTop
    , whenTop
    , implies
    , isTop
    , PBool(..)
    , PDualBool(..)
    )
    where

import Control.Concurrent.MVar

-- The imperative part

data MaybeTop
        = StillBottom (IO ()) -- ^ Just act: Still bottom, run act (once!) when triggered
        | SurelyTop           -- ^ Definitely top

newtype P2 = P2 (MVar MaybeTop)

newP2 :: IO P2
newP2 = P2 <$> newMVar (StillBottom (pure()))

newTopP2 :: IO P2
newTopP2 = P2 <$> newMVar SurelyTop

setTop ::  P2 -> IO ()
setTop (P2 p) = takeMVar p >>= \case
    SurelyTop -> putMVar p SurelyTop
    StillBottom act -> do
        -- Do this first, this breaks cycles
        putMVar p SurelyTop
        -- Now notify the dependencies
        act

whenTop :: P2 -> IO () -> IO ()
whenTop (P2 p1) act = takeMVar p1 >>= \case
    SurelyTop        -> putMVar p1 SurelyTop >> act
    StillBottom act' -> putMVar p1 (StillBottom (act >> act'))

implies :: P2 -> P2 -> IO ()
implies p1 p2 = whenTop p1 (setTop p2)

isTop :: P2 -> IO Bool
isTop (P2 p) = readMVar p >>= \case
    SurelyTop -> pure True
    StillBottom _ -> pure False

newtype PBool = PBool P2
newtype PDualBool = PDualBool P2
