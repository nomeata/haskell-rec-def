{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Data.Recursive.Propagator.Seminaive
where

import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad (unless)
import Control.Concurrent.MVar
import Data.IORef
-- import Data.Maybe

import Data.POrder

class (Eq a, Bottom a) => ChangeAction da a where
  -- laws:
  -- 1. update (kickoff x) bottom == x
  -- 2. (update dx x == y) implies ((x == y) == noChange dx x y)
  update :: da -> a -> a
  kickoff :: a -> da
  noChange :: da -> a -> a -> Bool
  noChange dx x y = x == y
class ChangeAction (Delta a) a => Change a where type Delta a

-- Might want to make this strict in both fields?
data Update a = Update { delta :: Delta a, new :: a }

-- Change propagator.
-- We assume each propagator has exactly one writer.
data Prop a = Prop
  { cell :: IORef a
  , lock :: MVar ()
  , listeners :: IORef [Update a -> IO ()]
  }

newProp :: a -> IO (Prop a)
newProp x = do
  cell <- newIORef x
  lock <- newMVar ()
  notify <- newIORef []
  pure $ Prop cell lock notify

updateProp :: Change a => Prop a -> IO (Delta a) -> IO ()
updateProp (Prop cell lock notify) func = do
  () <- takeMVar lock
  old <- readIORef cell
  delta <- func
  let new = update delta old
  let nop = noChange delta old new
  let u = Update delta new
  unless nop $ writeIORef cell new
  putMVar lock ()
  unless nop $ mapM_ ($ u) =<< readIORef notify

readProp = readIORef . cell

watchProp :: Change a => Prop a -> (Update a -> IO ()) -> IO ()
watchProp (Prop _ _ notify) f = atomicModifyIORef' notify (\fs -> (f:fs, ()))

initProp :: Change a => Prop a -> IO a -> IO ()
initProp p init = updateProp p $ do
                    old <- readProp p
                    new <- init
                    if old == bottom
                    then pure (kickoff new)
                    else error "Tried to re-initialize a propagator!"

changeProp :: Change a => Prop a -> Delta a -> IO ()
changeProp p delta = updateProp p (pure delta)

-- "simple" versions assume the derivative doesn't need access to anything but
-- deltas.
lift1simple :: (Change a, Change b) =>
               (a -> b) -> (Delta a -> Delta b) ->
               Prop a -> Prop b -> IO ()
lift1simple f df p1 p = do
  watchProp p1 $ changeProp p . df . delta
  initProp p $ f <$> readProp p1

lift2simple :: (Change a, Change b, Change c) =>
               (a -> b -> c) -> (Delta a -> Delta c) -> (Delta b -> Delta c) ->
               Prop a -> Prop b -> Prop c -> IO ()
lift2simple f df1 df2 p1 p2 p = do
  watchProp p1 $ changeProp p . df1 . delta
  watchProp p2 $ changeProp p . df2 . delta
  initProp p $ f <$> readProp p1 <*> readProp p2

-- Regular versions need an explicit state type.
updateRefWith :: IORef s -> (a -> s -> (b, s)) -> a -> IO b
updateRefWith ref f a = do (b, s) <- f a <$> readIORef ref
                           b <$ writeIORef ref s

lift1 :: (Change a, Change b) =>
         (a -> (b, state)) ->
         (Update a -> state -> (Delta b, state)) ->
         Prop a -> Prop b -> IO ()
lift1 f df p1 p = do
  state <- newIORef undefined
  initProp p $ do
    watchProp p1 $ updateProp p . updateRefWith state df
    (b, s) <- f <$> readProp p1
    b <$ writeIORef state s

lift2 :: (Change a, Change b, Change c) =>
         (a -> b -> (c, state)) ->
         (Update a -> state -> (Delta c, state)) ->
         (Update b -> state -> (Delta c, state)) ->
         Prop a -> Prop b -> Prop c -> IO ()
lift2 f df1 df2 p1 p2 p = do
  state <- newIORef undefined
  initProp p $ do
    watchProp p1 $ updateProp p . updateRefWith state df1
    watchProp p2 $ updateProp p . updateRefWith state df2
    (c, s) <- f <$> readProp p1 <*> readProp p2
    c <$ writeIORef state s
