-- | A copy of <https://hackage.haskell.org/package/Agda-2.6.3/docs/Agda-Utils-Parser-MemoisedCPS.html> with these changes
--
--  * Remove the ability to print the grammar, and remove the `ParserClass`
--  class
--  * Switch from unordered-containers to containers, to reduce dependencies
--  * Use `Any` and `unsafeCoerce` in `memoise` to remove the restriction that
--    all memoized productions need to have the same type. We cannot use
--    `Data.Dynamic` because we cannot afford extra constraints on `a`, else we
--    cannot instantiate `Functor` for the recursive parser.
--  * Removed `Monad` instance.  Unclear if you want that in the kind of parsers
--    we are looking at.

{-# LANGUAGE ScopedTypeVariables #-}
module Parser
  ( parse, sat', memoise
  , sat, token, tok
  , Parser
  ) where

import Control.Applicative ( Alternative((<|>), empty, many, some) )
import Control.Monad (liftM2, (<=<))
import Control.Monad.State.Strict (State, evalState, runState, get, modify')

import Data.Array
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.List as List
import Data.Maybe

import Text.PrettyPrint.HughesPJ hiding (empty)
import qualified Text.PrettyPrint.HughesPJ as PP

-- | Positions.

type Pos = Int

-- | State monad used by the parser.

type M k tok b = State (IntMap (Map k (Value k tok b)))

-- | Continuations.

type Cont k tok b a = Pos -> a -> M k tok b [b]

-- | Memoised values.

data Value k tok b = Value
  { _results       :: !(IntMap [Any])
  , _continuations :: [Cont k tok b Any]
  }

-- | The parser type.
--
-- The parameters of the type @Parser k tok a@ have the following
-- meanings:
--
-- [@k@] Type used for memoisation keys.
--
-- [@tok@] The token type.
--
-- [@a@] The result type.

newtype Parser k tok a =
  P { unP :: forall b.
             Array Pos tok ->
             Pos ->
             Cont k tok b a ->
             M k tok b [b]
    }

instance Monad (Parser k tok) where
  return    = pure
  P p >>= f = P $ \input i k ->
    p input i $ \j x -> unP (f x) input j k

instance Functor (Parser k tok) where
  fmap f (P p) = P $ \input i k ->
    p input i $ \i -> k i . f

instance Applicative (Parser k tok) where
  pure x        = P $ \_ i k -> k i x
  P p1 <*> P p2 = P $ \input i k ->
    p1 input i $ \i f ->
    p2 input i $ \i x ->
    k i (f x)

instance Alternative (Parser k tok) where
  empty         = P $ \_ _ _ -> return []
  P p1 <|> P p2 = P $ \input i k ->
    liftM2 (++) (p1 input i k) (p2 input i k)

parse :: Parser k tok a -> [tok] -> [a]
parse p toks =
  flip evalState IntMap.empty $
  unP p (listArray (0, n - 1) toks) 0 $ \j x ->
    if j == n then return [x] else return []
  where n = List.genericLength toks

sat' :: (tok -> Maybe a) -> Parser k tok a
sat' p = P $ \input i k ->
  if inRange (bounds input) i then
    case p (input ! i) of
      Nothing -> return []
      Just x  -> (k $! (i + 1)) $! x
  else
    return []

-- | Parses a token satisfying the given predicate.

sat :: (tok -> Bool) -> Parser k tok tok
sat p = sat' (\t -> if p t then Just t else Nothing)

-- | Parses a single token.

token :: Parser k tok tok
token = sat' Just

-- | Parses a given token.

tok :: Eq tok => tok -> Parser k tok tok
tok t = sat (t ==)

memoise :: forall k tok a. Ord k => k -> Parser k tok a -> Parser k tok a
memoise key p = P $ \input i k -> do

  let from :: Any -> a
      from = unsafeCoerce
  let to :: a -> Any
      to = unsafeCoerce
  let k' pos d = k pos (from d)

  let alter j zero f m =
        IntMap.alter (Just . f . fromMaybe zero) j m

      lookupTable   = fmap (Map.lookup key <=< IntMap.lookup i) get
      insertTable v = modify' $ alter i Map.empty (Map.insert key v)

  v <- lookupTable
  case v of
    Nothing -> do
      insertTable (Value IntMap.empty [k'])
      unP p input i $ \j r -> do
        ~(Just (Value rs ks)) <- lookupTable
        insertTable (Value (alter j [] (to r :) rs) ks)
        concat <$> mapM (\k -> k j (to r)) ks
    Just (Value rs ks) -> do
      insertTable (Value rs (k' : ks))
      concat . concat <$>
        mapM (\(i, rs) -> mapM (k i) (map from rs)) (IntMap.toList rs)
