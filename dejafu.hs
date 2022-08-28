import Test.DejaFu
import Control.Concurrent.Classy
import Control.Concurrent.Classy.Async
import qualified Data.Set as S
import System.Random

import Data.Recursive.Propagator.Naive

main = do
    putStrLn "Test 1"
    autocheck $ do
        p1 <- newProp (S.singleton 1)
        readProp p1

    putStrLn "Test 2"
    autocheck $ do
        p1 <- newProp (S.singleton 1)
        p2 <- newProp S.empty
        lift1 (S.insert 3) p1 p2
        mapConcurrently readProp [p1, p2]

    putStrLn "Test 2 rec"
    autocheck $ do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        mapConcurrently id
            [ lift1 (S.insert 3) p1 p2
            , lift1 (S.insert 4) p2 p1
            ]
        mapConcurrently readProp [p1, p2]

    putStrLn "Test 3 rec"
    autocheckWay (randomly (mkStdGen 0) 1000) defaultMemType $ do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        mapConcurrently id
            [ lift1 (S.insert 4) p1 p2
            , lift1 (S.insert 5) p2 p3
            , lift2 (S.union) p2 p3 p1
            ]
        mapConcurrently readProp [p1, p2, p3]

    putStrLn "Test 4"
    autocheckWay (randomly (mkStdGen 0) 1000) defaultMemType $ do
        p1 <- newProp S.empty
        p2 <- newProp S.empty
        p3 <- newProp S.empty
        p4 <- newProp S.empty
        mapConcurrently id
            [ lift1 (S.insert 4) p1 p2
            , lift2 (S.union) p1 p2 p3
            , liftList (S.unions) [p1,p2,p3] p4
            , lift1 (S.insert 5) p4 p1
            ]
        mapConcurrently readProp [p1, p2, p3, p4]
