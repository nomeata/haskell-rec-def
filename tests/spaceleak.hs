{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

{-|
This test checks that resolved cells do no longer hold on to references to
dependent cells, to avoid space leaks, especially with the “constant global”
cells like 'RS.empty'.
-}

module Main where

import qualified Data.Set as S
import qualified Data.Recursive.Set as RS
import Data.Recursive.Internal
import Data.Propagator.Purify

import Data.IORef
import System.Mem.Weak
import System.Mem
import System.Exit
import System.Environment
import Control.Concurrent

main = do
    n <- length <$> getArgs

    putStrLn "Test 1: Normal GC (sanity check)"
    gc <- newIORef False
    let x = 1000_000_001 + n
    addFinalizer x $ do
        putStrLn "Finalizer running"
        writeIORef gc True
    let s = RS.insert x s
    print (RS.get s)

    putStrLn "Running GC"
    performMajorGC
    threadDelay 1_000_000

    readIORef gc >>= \case
        True -> putStrLn "GC seems to be working"
        False -> putStrLn "This really ought to work" >> exitFailure


    putStrLn "Test 2: Dependency on RS.empty"
    gc <- newIORef False
    let x = 1000_000_002 + n
    addFinalizer x $ do
        putStrLn "Finalizer running"
        writeIORef gc True
    let s = RS.insert x RS.empty
    print (RS.get s)

    putStrLn "Running GC"
    performMajorGC
    threadDelay 1_000_000

    readIORef gc >>= \case
        True -> putStrLn "Good!"
        False -> putStrLn "We got a leak" >> exitFailure

    putStrLn "Test 3: Dependency on constant set"
    gc <- newIORef False
    let x0 = 1000_000_003 + n
    let s' = RS.mk (S.singleton x0)
    let x1 = 1000_000_004 + n
    addFinalizer x1 $ do
        putStrLn "Finalizer running"
        writeIORef gc True
    let s = RS.insert x1 s'
    print (RS.get s)

    putStrLn "Running GC"
    performMajorGC
    threadDelay 1_000_000

    readIORef gc >>= \case
        True -> putStrLn "Good!"
        False -> putStrLn "We got a leak" >> exitFailure

    putStrLn "Test 4: Dependency on recursive set"
    gc <- newIORef False
    let x0 = 1000_000_005 + n
    let sr = RS.insert x0 sr
    let x1 = 1000_000_007 + n
    addFinalizer x1 $ do
        putStrLn "Finalizer running"
        writeIORef gc True
    let s = RS.insert x1 sr
    print (RS.get s)

    putStrLn "Running GC"
    performMajorGC
    threadDelay 1_000_000

    readIORef gc >>= \case
        True -> putStrLn "Good!"
        False -> putStrLn "We got a leak" >> exitFailure

    -- This is needed, else RS.empty itself is GC’ed
    putStrLn "Keeping a few things alive"
    print (RS.get s')
    print (RS.get sr)
    print (RS.get RS.empty :: S.Set Int)

