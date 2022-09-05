import Test.DejaFu
import Control.Concurrent.Classy
import Control.Concurrent.Classy.Async
import qualified Data.Set as S
import System.Random
import Control.Monad
import Test.Tasty
import Test.Tasty.DejaFu

import Data.Recursive.Propagator.Naive
import Data.Recursive.Propagator.P2
import System.IO.RecThunk

t n = testAuto n

tr n = testAutoWay (randomly (mkStdGen 0) 1000) defaultMemType n

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
  , t "thunk 1" $ do
        obs1 <- newIORef 0
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            pure []
        force t1
        readIORef obs1
  , t "thunk 1 rec" $ do
        obs1 <- newIORef 0
        t1ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t1 <- readIORef t1ref
            pure [t1]
        writeIORef t1ref t1
        force t1
        readIORef obs1
  , t "thunk 2 rec 12" $ do
        obs1 <- newIORef 0
        obs2 <- newIORef 0
        t2ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t2 <- readIORef t2ref
            pure [t2]
        t2 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            pure [t1]
        writeIORef t2ref t2
        mapConcurrently id
            [ force t1 >> mapM readIORef [obs1, obs2]
            , force t2 >> mapM readIORef [obs1, obs2]
            ]
  , tr "thunk 2 rec 112" $ do
        obs1 <- newIORef 0
        obs2 <- newIORef 0
        t2ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t2 <- readIORef t2ref
            pure [t2]
        t2 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            pure [t1]
        writeIORef t2ref t2
        mapConcurrently id
            [ force t1 >> mapM readIORef [obs1, obs2]
            , force t1 >> mapM readIORef [obs1, obs2]
            , force t2 >> mapM readIORef [obs1, obs2]
            ]
  , tr "thunk 2 all-rec 112" $ do
        obs1 <- newIORef 0
        obs2 <- newIORef 0
        t1ref <- newIORef undefined
        t2ref <- newIORef undefined
        t1 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t1 <- readIORef t1ref
            t2 <- readIORef t2ref
            pure [t2,t1]
        writeIORef t1ref t1
        t2 <- thunk $ do
            atomicModifyIORef' obs1 (\x -> (succ x, ()))
            t2 <- readIORef t2ref
            pure [t1,t2]
        writeIORef t2ref t2
        mapConcurrently id
            [ force t1 >> mapM readIORef [obs1, obs2]
            , force t1 >> mapM readIORef [obs1, obs2]
            , force t2 >> mapM readIORef [obs1, obs2]
            ]
  , t "P2 1" $ do
    p1 <- newP2
    False <- isTop p1
    setTop p1
    True <- isTop p1
    pure ()
  , t "P2 2" $ do
    p1 <- newP2
    p2 <- newP2
    mapConcurrently id
        [ do
            False <- isTop p1
            setTop p1
            True <- isTop p1
            pure ()
        , do
            False <- isTop p2
            p1 `implies` p2
        ]
    True <- isTop p2
    pure ()
  , t "P2 2 rec bottom" $ do
    p1 <- newP2
    p2 <- newP2
    mapConcurrently id
        [  p1 `implies` p2
        ,  p2 `implies` p1
        ]
    [False, False] <- mapM isTop [p1,p2]
    pure ()
  , t "P2 2 rec top" $ do
    p1 <- newP2
    p2 <- newP2
    mapConcurrently id
        [  p1 `implies` p2
        ,  p2 `implies` p1
        , setTop p1
        ]
    [True, True] <- mapM isTop [p1,p2]
    pure ()
  ]
