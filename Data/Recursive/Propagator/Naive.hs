{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

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


-- I want to test this code with dejafu, without carrying it as a dependency
-- of the main library. So here is a bit of CPP to care for that.

#ifndef MIN_VERSION_dejafu

import Control.Concurrent.MVar
import Control.Monad

data Prop a = Prop
    { val :: MVar a
    , onChange :: MVar (IO ())
    }

newProp :: a -> IO (Prop a)
readProp :: Prop a -> IO a
setProp :: Eq a => Prop a -> a -> IO ()
watchProp :: Prop a -> IO () -> IO ()
lift1 :: Eq b => (a -> b) -> Prop a -> Prop b -> IO ()
lift2 :: Eq c => (a -> b -> c) -> Prop a -> Prop b -> Prop c -> IO ()
liftList :: Eq b => ([a] -> b) -> [Prop a] -> Prop b -> IO ()
mkUpdate :: IO () -> IO (IO ())

#else

import Control.Concurrent.Classy
import Control.Monad

data Prop m a = Prop
    { val :: MVar m a
    , onChange :: MVar m (m ())
    }

newProp :: MonadConc m => a -> m (Prop m a)
readProp :: MonadConc m => Prop m a -> m a
setProp :: MonadConc m => Eq a => Prop m a -> a -> m ()
watchProp :: MonadConc m => Prop m a -> m () -> m ()
lift1 :: MonadConc m => Eq b => (a -> b) -> Prop m a -> Prop m b -> m ()
lift2 :: MonadConc m => Eq c => (a -> b -> c) -> Prop m a -> Prop m b -> Prop m c -> m ()
liftList :: MonadConc m => Eq b => ([a] -> b) -> [Prop m a] -> Prop m b -> m ()
mkUpdate :: MonadConc m => m () -> m (m ())

#endif


newProp x = do
    m <- newMVar x
    notify <- newMVar (pure ())
    pure $ Prop m notify

readProp (Prop m _ ) = readMVar m

setProp (Prop m notify) x = do
    old <- swapMVar m x
    unless (old == x) $ join (readMVar notify)

watchProp (Prop m notify) f =
    modifyMVar_ notify $ \a -> pure (f >> a)

lift1 f p1 p = do
    update <- mkUpdate $ do
        x <- readProp p1
        setProp p (f x)
    watchProp p1 update
    update

lift2 f p1 p2 p = do
    update <- mkUpdate $ do
        x <- readProp p1
        y <- readProp p2
        setProp p (f x y)
    watchProp p1 update
    watchProp p2 update
    update

liftList f ps p = do
    update <- mkUpdate $ do
        xs <- mapM readProp ps
        setProp p (f xs)
    mapM_ (\p' -> watchProp p' update) ps
    update


data Todo = Done | Doing | Todo

mkUpdate act = do
    lock <- newMVar Done
    let go = do
            act
            takeMVar lock >>= \case
                Done  -> error "Someone else done it?"
                Doing -> putMVar lock Done
                Todo  -> putMVar lock Doing >> go
    return $ takeMVar lock >>= \case
        Done  -> putMVar lock Doing >> go
        Doing -> putMVar lock Todo
        Todo  -> putMVar lock Todo

