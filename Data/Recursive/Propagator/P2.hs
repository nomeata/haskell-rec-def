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

-- | A type for propagators for the two-point lattice, consisting of bottom and top
newtype P2 = P2 (MVar MaybeTop)

-- | A new propagator, initialized at bottom
newP2 :: IO P2
newP2 = P2 <$> newMVar (StillBottom (pure()))

-- | A new propagator, already set to top
newTopP2 :: IO P2
newTopP2 = P2 <$> newMVar SurelyTop

-- | @whenTop p act@ runs @act@ if @p@ is already top, or after @setTop p@ is run
whenTop :: P2 -> IO () -> IO ()
whenTop (P2 p1) act = takeMVar p1 >>= \case
    SurelyTop        -> putMVar p1 SurelyTop >> act
    StillBottom act' -> putMVar p1 (StillBottom (act >> act'))


-- | Set a propagator to top.
--
-- If it was bottom before, runs the actions queued with 'whenTop'. It does so
-- _after_ setting the propagator to top, so that cycles are broken.
setTop ::  P2 -> IO ()
setTop (P2 p) = takeMVar p >>= \case
    SurelyTop -> putMVar p SurelyTop
    StillBottom act -> do
        -- Do this first, this breaks cycles
        putMVar p SurelyTop
        -- Now notify the dependencies
        act

-- | @p1 `implies` p2@ chains propagators: If @p1@ becomes top, then so does @p2@.
implies :: P2 -> P2 -> IO ()
implies p1 p2 = whenTop p1 (setTop p2)

-- | Queries the current state of the propagator. All related calls to @setTop@
-- that have executed so fa are taken into account.
isTop :: P2 -> IO Bool
isTop (P2 p) = readMVar p >>= \case
    SurelyTop -> pure True
    StillBottom _ -> pure False

-- | A newtype around 'P2' to denote that bottom is 'False' and top is 'True'
newtype PBool = PBool P2

-- | A newtype around 'P2' to denote that bottom is 'True' and top is 'False'
newtype PDualBool = PDualBool P2
