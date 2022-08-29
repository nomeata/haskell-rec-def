import Test.DejaFu
import Control.Concurrent.Classy
import Control.Concurrent.Classy.Async
import qualified Data.Set as S
import System.Random
import Control.Monad
import Test.Tasty
import Test.Tasty.DejaFu

import Data.Recursive.Propagator.Naive
import System.IO.RecThunk

t n = testGroup n . pure . testAuto

tr n = testGroup n . pure . testAutoWay (randomly (mkStdGen 0) 1000) defaultMemType

main = defaultMain $ testGroup "tests" $
  [ t "prop 1" $ do
        p1 <- newProp (S.singleton 1)
        readProp p1

  , t "prop 2" $ do
        p1 <- newProp (S.singleton 1)
        p2 <- newProp S.empty
        lift1 (S.insert 3) p1 p2
        mapConcurrently readProp [p1, p2]

  , tr "prop 2 rec" $ withSetup (do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        pure (p1, p2)) $ \(p1, p2) -> do
        mapConcurrently id
            [ lift1 (S.insert 3) p1 p2
            , lift1 (S.insert 4) p2 p1
            ]
        mapConcurrently readProp [p1, p2]

  , tr "prop 2 rec plus" $ withSetup (do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        pure (p1, p2, p3)) $ \(p1, p2, p3) -> do
        mapConcurrently id
            [ lift1 (S.insert 3) p1 p2
            , lift1 (S.insert 4) p2 p1
            ]
        mapConcurrently id
            [ readProp p1
            , readProp p2
            , lift1 (S.insert 5) p2 p3 >> readProp p3
            ]


  , tr "prop 3 rec" $ withSetup (do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        pure (p1, p2, p3)) $ \(p1, p2, p3) -> do
        mapConcurrently id
            [ lift1 (S.insert 3) p1 p2
            , lift1 (S.insert 4) p2 p1
            , lift1 (S.insert 5) p2 p3
            ]
        mapConcurrently readProp [p1, p2, p3]

  , tr "prop 3 rec variant" $ withSetup (do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        pure (p1, p2, p3)) $ \(p1, p2, p3) -> do
        mapConcurrently id
            [ lift1 (S.insert 4) p1 p2
            , lift1 (S.insert 5) p2 p3
            , lift2 (S.union) p2 p3 p1
            ]
        mapConcurrently readProp [p1, p2, p3]

  , tr "prop 4 rec" $ withSetup (do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        p4 <- newProp S.empty
        pure (p1, p2, p3, p4)) $ \(p1, p2, p3, p4) -> do
        mapConcurrently id
            [ lift1 (S.insert 4) p1 p2
            , lift2 (S.union) p1 p2 p3
            , liftList (S.unions) [p1,p2,p3] p4
            , lift1 (S.insert 5) p4 p1
            ]
        mapConcurrently readProp [p1, p2, p3, p4]
  , tr "thunk 1" $ do
        obs1 <- newIORef 0
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            pure []
        force t1
        readIORef obs1
  , tr "thunk 1 rec" $ do
        obs1 <- newIORef 0
        t1ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t1 <- readIORef t1ref
            mapM kick [t1]
        writeIORef t1ref t1
        force t1
        readIORef obs1
  , tr "thunk 2 rec 112" $ do
        obs1 <- newIORef 0
        obs2 <- newIORef 0
        t2ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t2 <- readIORef t2ref
            mapM kick [t2]
        t2 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            mapM kick [t1]
        writeIORef t2ref t2
        mapConcurrently id
            [ force t1 >> mapM readIORef [obs1, obs2]
            , force t1 >> mapM readIORef [obs1, obs2]
            , force t2 >> mapM readIORef [obs1, obs2]
            ]
  ]
