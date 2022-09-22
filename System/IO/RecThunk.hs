{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|

The 'Thunk' API provides a way to defer potentially recursive computations:

* 'thunk' is lazy in its argument, and does not run it directly
* the first 'force' triggers execution of the action passed to thunk
* that action is run at most once, and returns a list of other thunks
* 'force' forces these thunks as well, and does not return before all of them have executed
* Cycles are allowed: The action passed to 'thunk' may return a thunk whose action returns the first thunk.

The implementation is hopefully thread safe: Even if multiple threads force or
kick related thunks, all actions are still run at most once, and all calls to
force terminate (no deadlock).

>>> :set -XRecursiveDo
>>> :{
  mdo t1 <- thunk $ putStrLn "Hello" >> pure [t1, t2]
      t2 <- thunk $ putStrLn "World" >> pure [t1, t2]
      putStrLn "Nothing happened so far, but now:"
      force t1
      putStrLn "No more will happen now:"
      force t1
      putStrLn "That's it"
:}
Nothing happened so far, but now:
Hello
World
No more will happen now:
That's it

-}
module System.IO.RecThunk
    ( Thunk
    , thunk
    , doneThunk
    , force
    )
where


-- I want to test this code with dejafu, without carrying it as a dependency
-- of the main library. So here is a bit of CPP to care for that.

#ifdef DEJAFU

#define Ctxt   (MonadConc m, MonadIO m) =>
#define Thunk_  (Thunk m)
#define ResolvingState_  (ResolvingState m)
#define KickedThunk_  (KickedThunk m)
#define MVar_  MVar m
#define M      m

import Control.Concurrent.Classy hiding (wait)
import Data.Unique
import Control.Monad.IO.Class

#else

#define Ctxt
#define Thunk_  Thunk
#define ResolvingState_  ResolvingState
#define KickedThunk_  KickedThunk
#define MVar_  MVar
#define M      IO

import Control.Concurrent.MVar
import Data.Unique
import Control.Monad.IO.Class

#endif

-- | An @IO@ action that is to be run at most once
newtype Thunk_ = Thunk (MVar_ (Either (M [Thunk_]) KickedThunk_))
data ResolvingState_ = NotStarted | ProcessedBy Unique (MVar_ ()) | Done
-- | A 'Thunk' that is being evaluated
data KickedThunk_ = KickedThunk (MVar_ [KickedThunk_]) (MVar_ ResolvingState_)

-- | Create a new 'Thunk' from an 'IO' action.
--
-- The 'IO' action may return other thunks that should be forced together
-- whenver this thunk is forced (in arbitrary order)
thunk :: Ctxt M [Thunk_] -> M Thunk_
thunk act = Thunk <$> newMVar (Left act)

-- | A Thunk that that already is done.
--
-- Equivalent to @do {t <- thunk (pure []); force t; pure t }@
doneThunk :: Ctxt M Thunk_
doneThunk = do
    mv_ts <- newMVar []
    mv_s <- newMVar Done
    Thunk <$> newMVar (Right (KickedThunk mv_ts mv_s))

-- Recursively explores the thunk, and kicks the execution
-- May return before before execution is done (if started by another thread)
kick :: Ctxt Thunk_ -> M KickedThunk_
kick (Thunk t) = takeMVar t >>= \case
    Left act -> do
        mv_thunks <- newEmptyMVar
        mv_state <- newMVar NotStarted
        let kt = KickedThunk mv_thunks mv_state
        putMVar t (Right kt)

        ts <- act
        kts <- mapM kick ts
        putMVar mv_thunks kts
        pure kt

    -- Thread was already kicked, nothing to do
    Right kt -> do
        putMVar t (Right kt)
        pure kt

wait :: Ctxt Unique -> KickedThunk_ -> M ()
wait my_id (KickedThunk mv_deps mv_s) = do
    s <- takeMVar mv_s
    case s of
        -- Thunk and all dependences are done
        Done -> putMVar mv_s s
        -- Thunk is being processed by a higher priority thread, so simply wait
        ProcessedBy other_id done_mv | other_id < my_id -> do
            putMVar mv_s s
            readMVar done_mv
        -- Thunk is already being processed by this thread, ignore
        ProcessedBy other_id _done_mv | other_id == my_id -> do
            putMVar mv_s s
            pure ()
        -- Thunk is not yet processed, or processed by a lower priority thread, so process now
        _ -> do
            done_mv <- newEmptyMVar
            putMVar mv_s (ProcessedBy my_id done_mv)
            ts <- readMVar mv_deps
            mapM_ (wait my_id) ts
            -- Mark kicked thunk as done
            _ <- swapMVar mv_s Done
            -- Wake up waiting threads
            putMVar done_mv ()

-- | Force the execution of the thunk. If it has been forced already, it will
-- do nothing. Else it will run the action passed to 'thunk', force thunks
-- returned by that action, and not return until all of them are forced.
force :: Ctxt Thunk_ -> M ()
force t = do
    rt <- kick t
    my_id <- liftIO newUnique
    wait my_id rt
