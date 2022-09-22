{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- This module provides the 'Purify' data type, which wraps a imperative propagator
-- (for example "Data.Propagator.Naive") in a pure data structure.
--
-- It provides functions to declare the inputs to these propagators, which are unsafe on their own, but can be instantiated and wrapped to form safe APIs, e.g. "Data.Recursive.Bool".
--
-- This module is labeled as Internal because its safety depends on the behaviour of the
-- underlying propagator implementation. The assumptions is that
--
-- * The defining function passed to `def1` etc. declare a functional relation
--   between the input propagators and the output propagator.
-- * Defining functions do not (observably) affect their input propagators.
-- * Once all the functions passed to `def1` of a propagator and its
--   dependencies have run, `readProp` will return a correct value, i.e. one
--   that satisfies the functional relations.
-- * The order in which the defining functions are executed does not affect the
--   result.
-- * Termination depends on the termination of the underlying propagator
--
module Data.Propagator.Purify
    ( Purify
    , get
    , mk, def1, def2, defList
    )
where

import System.IO.Unsafe

import Data.Propagator.Class
import System.IO.RecThunk

-- | A value of type @Purify p@ is a propagator @p@, gether with a (lazy)
-- action to define it.
--
-- You can use 'get' to extract the value from the propagator.
--
-- Do not use the extracted value in the definition of that value, this will
-- loop just like a recursive definition with plain values would!
data Purify p = Purify
        { prop :: p
        , pre :: Thunk
        , post :: Thunk
        }

-- | Any value of type @a@ is also a value of type @Purify p@ if @p@ is a propagator for @a@.
mk :: Propagator p a => a -> Purify p
mk x = unsafePerformIO $ do
    p <- newConstProp x
    t1 <- doneThunk
    t2 <- doneThunk
    pure (Purify p t1 t2)

new :: Propagator p a => [Thunk] -> [Thunk] -> (p -> IO ()) -> Purify p
new ts1 ts2 act = unsafePerformIO $ do
    p <- newProp
    t1 <- thunk $ act p >> pure ts1
    t2 <- thunk $ freezeProp p >> pure ts2
    pure (Purify p t1 t2)

-- | Defines a value of type @Purify b@ to be a function of the values of @Purify a@.
--
-- The action passed should declare that relation to the underlying propagator.
--
-- The @Prop a@ propagator must only be used for reading values /from/.
def1 :: (Propagator pa a, Propagator pb b) =>
    (pa -> pb -> IO ()) ->
    Purify pa -> Purify pb
def1 def r1 = new [pre r1] [post r1] $ \p -> do
    def (prop r1) p

-- | Defines a value of type @Purify c@ to be a function of the values of @Purify a@ and @Purify b@.
--
-- The action passed should declare that relation to the underlying propagator.
--
-- The @Prop a@ and @Prop b@ propagators must only be used for reading values /from/.
def2 :: (Propagator pa a, Propagator pb b, Propagator pc c) =>
    (pa -> pb -> pc -> IO ()) ->
    Purify pa -> Purify pb -> Purify pc
def2 def r1 r2 = new [pre r1, pre r2] [post r1, post r2] $ \p -> do
    def (prop r1) (prop r2) p

-- | Defines a value of type @Purify b@ to be a function of the values of a list of @Purify a@ values.
--
-- The action passed should declare that relation to the underlying propagator.
--
-- The @Prop a@ propagators must only be used for reading values /from/.
defList :: (Propagator pa a, Propagator pb b) =>
    ([pa] -> pb -> IO ()) ->
    [Purify pa] -> Purify pb
defList def rs = new (map pre rs) (map post rs) $ \p -> do
    def (map prop rs) p

-- | Extract the value from a @Purify a@. This must not be used when /defining/ that value.
get :: Propagator pa a => Purify pa -> a
get r = unsafePerformIO $ do
    force (pre r)
    force (post r)
    readProp (prop r)
