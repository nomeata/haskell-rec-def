{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A very naive propagator library.
--
-- This propagator implementation keeps updating the values accoring to their
-- definitions as other values change, until a fixed-point is reached.
--
-- It is a naive implementation and not very clever. Much more efficient
-- propagator implementations are possible, and may be used by this library in
-- the future.
module Data.Propagator.Naive
    ( Prop
    , newProp
    , newConstProp
    , freezeProp
    , readProp
    , watchProp
    , setProp
    , lift1
    , lift2
    , liftList
    )
    where

import Control.Monad
import Data.POrder
import Data.Maybe

import qualified Data.Propagator.Class as Class

-- I want to test this code with dejafu, without carrying it as a dependency
-- of the main library. So here is a bit of CPP to care for that.

#ifdef DEJAFU

#define Ctxt   MonadConc m =>
#define Prop_  Prop m
#define IORef_ IORef m
#define MVar_  MVar m
#define M      m

import Control.Concurrent.Classy

#else

#define Ctxt
#define Prop_  Prop
#define IORef_ IORef
#define MVar_  MVar
#define M      IO

import Control.Exception
import Control.Concurrent.MVar
import Data.IORef

#endif

-- | A cell in a propagator network
data Prop_ a = Prop
    { _val :: IORef_ a
    , _lock :: MVar_ ()
    , _onChange :: IORef_ (Maybe (M ()))
    }

-- | Creates a cell, initialized to bottom
newProp :: Ctxt a -> M (Prop_ a)
newProp x = do
    m <- newIORef x
    l <- newMVar ()
    notify <- newIORef (Just (pure ()))
    pure $ Prop m l notify

-- | Creates a constant cell, given an initial value
newConstProp :: Ctxt a -> M (Prop_ a)
newConstProp x = do
    m <- newIORef x
    l <- newMVar ()
    notify <- newIORef Nothing
    pure $ Prop m l notify

-- | Reads the current value of the cell
readProp :: Ctxt Prop_ a -> M a
readProp (Prop m _ _ ) = readIORef m

-- | Is the current propagator already frozen?
isFrozen :: Ctxt Prop_ a -> M Bool
isFrozen (Prop _ _ notify) = do
    isNothing <$> readIORef notify

-- | Marks the propagator as frozen.
--
-- Will prevent further calls to setProp and clears the list of watchers (to
-- allow GC).
freezeProp :: Ctxt Prop_ a -> M ()
freezeProp (Prop _ _ notify) = do
    writeIORef notify Nothing

-- | Sets a new value calculated from the given action. The action is executed atomically.
--
-- Throws if the propagator is already frozen
--
-- If the value has changed, all watchers are notified afterwards (not atomically).
setProp :: Ctxt POrder a => Prop_ a -> M a -> M ()
setProp p@(Prop m l notify) getX = do
    frozen <- isFrozen p
    when frozen $ throw Class.WriteToFrozenPropagatorException
    () <- takeMVar l
    old <- readIORef m
    new <- getX
    writeIORef m new
    putMVar l ()
    unless (old `eqOfLe` new) $
        readIORef notify >>= \case
            Nothing -> pure ()
            Just act -> act

-- | Watch a cell: If the value changes, the given action is executed
watchProp :: Ctxt Prop_ a -> M () -> M ()
watchProp (Prop _ _ notify) f =
    atomicModifyIORef notify $ \case
        Nothing -> (Nothing, ())
        Just a -> (Just (f >> a), ())

-- | Whenever the first cell changes, update the second, using the given function
lift1 :: Ctxt POrder b => (a -> b) -> Prop_ a -> Prop_ b -> M ()
lift1 f p1 p = do
    let update = setProp p $ f <$> readProp p1
    watchProp p1 update
    update

-- | Whenever any of the first two cells change, update the third, using the given function
lift2 :: Ctxt POrder c => (a -> b -> c) -> Prop_ a -> Prop_ b -> Prop_ c -> M ()
lift2 f p1 p2 p = do
    let update = setProp p $ f <$> readProp p1 <*> readProp p2
    watchProp p1 update
    watchProp p2 update
    update

-- | Whenever any of the cells in the list change, update the other, using the given function
liftList :: Ctxt POrder b => ([a] -> b) -> [Prop_ a] -> Prop_ b -> M ()
liftList f ps p = do
    let update = setProp p $ f <$> mapM readProp ps
    mapM_ (\p' -> watchProp p' update) ps
    update

#ifndef DEJAFU
instance Bottom a => Class.Propagator (Prop_ a) a where
    newProp = newProp bottom
    newConstProp = newConstProp
    freezeProp = freezeProp
    readProp = readProp
#endif
