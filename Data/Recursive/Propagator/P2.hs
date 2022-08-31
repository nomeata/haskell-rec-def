{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

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

-- I want to test this code with dejafu, without carrying it as a dependency
-- of the main library. So here is a bit of CPP to care for that.

#ifdef DEJAFU

#define Ctxt   MonadConc m =>
#define MaybeTop_  (MaybeTop m)
#define P2_  (P2 m)
#define PBool_  PBool m
#define PDualBool_  PDualBool m
#define IORef_ IORef m
#define MVar_  MVar m
#define M      m

import Control.Concurrent.Classy

#else

#define Ctxt
#define MaybeTop_  MaybeTop
#define P2_  P2
#define PBool_  PBool
#define PDualBool_  PDualBool
#define IORef_ IORef
#define MVar_  MVar
#define M      IO

import Control.Concurrent.MVar
import Data.IORef

#endif

data MaybeTop_
        = StillBottom (M ()) -- ^ Just act: Still bottom, run act (once!) when triggered
        | SurelyTop           -- ^ Definitely top

-- | A type for propagators for the two-point lattice, consisting of bottom and top
newtype P2_ = P2 (MVar_ MaybeTop_)

-- | A new propagator, initialized at bottom
newP2 :: Ctxt M P2_
newP2 = P2 <$> newMVar (StillBottom (pure()))

-- | A new propagator, already set to top
newTopP2 :: Ctxt M P2_
newTopP2 = P2 <$> newMVar SurelyTop

-- | @whenTop p act@ runs @act@ if @p@ is already top, or after @setTop p@ is run
whenTop :: Ctxt P2_ -> M () -> M ()
whenTop (P2 p1) act = takeMVar p1 >>= \case
    SurelyTop        -> putMVar p1 SurelyTop >> act
    StillBottom act' -> putMVar p1 (StillBottom (act >> act'))


-- | Set a propagator to top.
--
-- If it was bottom before, runs the actions queued with 'whenTop'. It does so
-- _after_ setting the propagator to top, so that cycles are broken.
setTop :: Ctxt P2_ -> M ()
setTop (P2 p) = takeMVar p >>= \case
    SurelyTop -> putMVar p SurelyTop
    StillBottom act -> do
        -- Do this first, this breaks cycles
        putMVar p SurelyTop
        -- Now notify the dependencies
        act

-- | @p1 `implies` p2@ chains propagators: If @p1@ becomes top, then so does @p2@.
implies :: Ctxt P2_ -> P2_ -> M ()
implies p1 p2 = whenTop p1 (setTop p2)

-- | Queries the current state of the propagator. All related calls to @setTop@
-- that have executed so far are taken into account.
isTop :: Ctxt P2_ -> M Bool
isTop (P2 p) = readMVar p >>= \case
    SurelyTop -> pure True
    StillBottom _ -> pure False

-- | A newtype around 'P2' to denote that bottom is 'False' and top is 'True'
newtype PBool_ = PBool P2_

-- | A newtype around 'P2' to denote that bottom is 'True' and top is 'False'
newtype PDualBool_ = PDualBool P2_
