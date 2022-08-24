module Data.Recursive.Propagator where

import Control.Concurrent.MVar
import Control.Monad

data Prop a = Prop
    { val :: MVar a
    , onChange :: MVar (IO ())
    }

newProp :: a -> IO (Prop a)
newProp x = do
    m <- newMVar x
    notify <- newMVar (pure ())
    pure $ Prop m notify

setProp :: Eq a => Prop a -> a -> IO ()
setProp (Prop m notify) x = do
    old <- swapMVar m x
    unless (old == x) $ join (readMVar notify)

readProp :: Prop a -> IO a
readProp (Prop m _ ) = readMVar m

watchProp :: Prop a -> IO () -> IO ()
watchProp (Prop m notify) f = 
    modifyMVar_ notify $ \a -> pure (f >> a)

-- TODO: Is this implementation thread-safe?

lift1 :: Eq b => (a -> b) -> Prop a -> Prop b -> IO ()
lift1 f p1 p = do
    watchProp p1 update
    update
  where
    update = do
        x <- readProp p1
        setProp p (f x)

lift2 :: Eq c => (a -> b -> c) -> Prop a -> Prop b -> Prop c -> IO ()
lift2 f p1 p2 p = do
    watchProp p1 update
    watchProp p2 update
    update
  where
    update = do
        x <- readProp p1
        y <- readProp p2
        setProp p (f x y)

liftList :: Eq b => ([a] -> b) -> [Prop a] -> Prop b -> IO ()
liftList f ps p = do
    mapM_ (\p' -> watchProp p' update) ps
    update
 where
    update = do
        xs <- mapM readProp ps
        setProp p (f xs)

