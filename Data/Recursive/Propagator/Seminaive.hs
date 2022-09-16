{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Data.Recursive.Propagator.Seminaive
where

import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad (unless)
import Control.Concurrent.MVar
import Data.IORef

class Eq a => ChangeAction da a where
  -- laws:
  -- 1. update (diff y x) x == y
  -- 2. noop dx x == (x == update dx x)
  update :: da -> a -> a
  diff :: a -> a -> da
  noop :: da -> a -> Bool
  noop dx x = x == update dx x
class ChangeAction (Delta a) a => Change a where type Delta a

data Update a = Update { old :: a, new :: a, delta :: Delta a }

-- Change propagator.
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

-- readProp :: Prop a -> IO a
-- readProp (Prop m _ _) = readIORef m

modifyProp :: Change a => Prop a -> (a -> Delta a) -> IO ()
modifyProp (Prop cell lock notify) func = do
  () <- takeMVar lock
  old <- readIORef cell
  let delta = func old
  let nop = noop delta old
  let new = update delta old
  let u = Update old new delta
  unless nop $ writeIORef cell new
  putMVar lock ()
  unless nop $ mapM_ ($ u) =<< readIORef notify

setProp :: Change a => Prop a -> a -> IO ()
setProp p new = modifyProp p (diff new)

updateProp :: Change a => Prop a -> Delta a -> IO ()
updateProp p delta = modifyProp p (const delta)

watchProp :: Change a => Prop a -> (Update a -> IO ()) -> IO ()
watchProp (Prop _ _ notify) f = atomicModifyIORef' notify (\fs -> (f:fs, ()))

readProp = readIORef . cell

lift1 :: (Change a, Change b) =>
         (a -> b) -> (Update a -> Delta b) ->
         Prop a -> Prop b -> IO ()
lift1 f df p1 p = do
  -- Not confident about lack of races here.
  watchProp p1 $ updateProp p . df
  setProp p . f =<< readProp p1

lift2 :: (Change a, Change b, Change c) =>
         (a -> b -> c) ->
         (Update a -> b -> Delta c) ->
         (a -> Update b -> Delta c) ->
         Prop a -> Prop b -> Prop c -> IO ()
lift2 f df1 df2 p1 p2 p = do
  watchProp p1 $ \da -> updateProp p =<< df1 da <$> readProp p2
  watchProp p2 $ \db -> updateProp p =<< flip df2 db <$> readProp p1
  setProp p =<< f <$> readProp p1 <*> readProp p2

-- liftList :: (Change a, Change b) =>
--             ([a] -> b) ->
--             (Int -> [a] -> Update a ->

