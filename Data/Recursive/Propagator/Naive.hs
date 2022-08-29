{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A very naive propagator library
--
module Data.Recursive.Propagator.Naive
    ( Prop
    , newProp
    , readProp
    , watchProp
    , setProp
    , lift1
    , lift2
    , liftList
    )
    where

import Control.Monad

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

import Control.Concurrent.MVar
import Data.IORef

#endif


data Prop_ a = Prop
    { val :: IORef_ a
    , lock :: MVar_ ()
    , onChange :: IORef_ (M ())
    }

newProp :: Ctxt a -> M (Prop_ a)
newProp x = do
    m <- newIORef x
    l <- newMVar ()
    notify <- newIORef (pure ())
    pure $ Prop m l notify

readProp :: Ctxt Prop_ a -> M a
readProp (Prop m _ _ ) = readIORef m

setProp :: Ctxt Eq a => Prop_ a -> M a -> M ()
setProp (Prop m l notify) getX = do
    () <- takeMVar l
    old <- readIORef m
    new <- getX
    writeIORef m new
    putMVar l ()
    unless (new == old) $ join (readIORef notify)

watchProp :: Ctxt Prop_ a -> M () -> M ()
watchProp (Prop _ _ notify) f =
    atomicModifyIORef notify $ \a -> (f >> a, ())

lift1 :: Ctxt Eq b => (a -> b) -> Prop_ a -> Prop_ b -> M ()
lift1 f p1 p = do
    let update = setProp p $ f <$> readProp p1
    watchProp p1 update
    update

lift2 :: Ctxt Eq c => (a -> b -> c) -> Prop_ a -> Prop_ b -> Prop_ c -> M ()
lift2 f p1 p2 p = do
    let update = setProp p $ f <$> readProp p1 <*> readProp p2
    watchProp p1 update
    watchProp p2 update
    update

liftList :: Ctxt Eq b => ([a] -> b) -> [Prop_ a] -> Prop_ b -> M ()
liftList f ps p = do
    let update = setProp p $ f <$> mapM readProp ps
    mapM_ (\p' -> watchProp p' update) ps
    update
