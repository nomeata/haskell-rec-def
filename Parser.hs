-- | A copy of <https://hackage.haskell.org/package/Agda-2.6.3/docs/Agda-Utils-Parser-MemoisedCPS.html> with these changes
--
--  * Remove the ability to print the grammar, and remove the `ParserClass`
--  class
--  * Switch from unordered-containers to containers, to reduce dependencies

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


import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.List as List
import Data.Maybe

import Text.PrettyPrint.HughesPJ hiding (empty)
import qualified Text.PrettyPrint.HughesPJ as PP

-- | Positions.

type Pos = Int

-- | State monad used by the parser.

type M k r tok b = State (IntMap (Map k (Value k r tok b)))

-- | Continuations.

type Cont k r tok b a = Pos -> a -> M k r tok b [b]

-- | Memoised values.

data Value k r tok b = Value
  { _results       :: !(IntMap [r])
  , _continuations :: [Cont k r tok b r]
  }

-- | The parser type.
--
-- The parameters of the type @Parser k r tok a@ have the following
-- meanings:
--
-- [@k@] Type used for memoisation keys.
--
-- [@r@] The type of memoised values. (Yes, all memoised values have
-- to have the same type.)
--
-- [@tok@] The token type.
--
-- [@a@] The result type.

newtype Parser k r tok a =
  P { unP :: forall b.
             Array Pos tok ->
             Pos ->
             Cont k r tok b a ->
             M k r tok b [b]
    }

instance Monad (Parser k r tok) where
  return    = pure
  P p >>= f = P $ \input i k ->
    p input i $ \j x -> unP (f x) input j k

instance Functor (Parser k r tok) where
  fmap f (P p) = P $ \input i k ->
    p input i $ \i -> k i . f

instance Applicative (Parser k r tok) where
  pure x        = P $ \_ i k -> k i x
  P p1 <*> P p2 = P $ \input i k ->
    p1 input i $ \i f ->
    p2 input i $ \i x ->
    k i (f x)

instance Alternative (Parser k r tok) where
  empty         = P $ \_ _ _ -> return []
  P p1 <|> P p2 = P $ \input i k ->
    liftM2 (++) (p1 input i k) (p2 input i k)

-- | Parses a token satisfying the given predicate.

sat :: (tok -> Bool) -> Parser k r tok tok
sat p = sat' (\t -> if p t then Just t else Nothing)

-- | Parses a single token.

token :: Parser k r tok tok
token = sat' Just

-- | Parses a given token.

tok :: Eq tok => tok -> Parser k r tok tok
tok t = sat (t ==)

parse :: Parser k r tok a -> [tok] -> [a]
parse p toks =
  flip evalState IntMap.empty $
  unP p (listArray (0, n - 1) toks) 0 $ \j x ->
    if j == n then return [x] else return []
  where n = List.genericLength toks

sat' :: (tok -> Maybe a) -> Parser k r tok a
sat' p = P $ \input i k ->
  if inRange (bounds input) i then
    case p (input ! i) of
      Nothing -> return []
      Just x  -> (k $! (i + 1)) $! x
  else
    return []

memoise :: (Ord k) => k -> Parser k r tok r -> Parser k r tok r
memoise key p = P $ \input i k -> do

  let alter j zero f m =
        IntMap.alter (Just . f . fromMaybe zero) j m

      lookupTable   = fmap (Map.lookup key <=< IntMap.lookup i) get
      insertTable v = modify' $ alter i Map.empty (Map.insert key v)

  v <- lookupTable
  case v of
    Nothing -> do
      insertTable (Value IntMap.empty [k])
      unP p input i $ \j r -> do
        ~(Just (Value rs ks)) <- lookupTable
        insertTable (Value (alter j [] (r :) rs) ks)
        concat <$> mapM (\k -> k j r) ks
    Just (Value rs ks) -> do
      insertTable (Value rs (k : ks))
      concat . concat <$>
        mapM (\(i, rs) -> mapM (k i) rs) (IntMap.toList rs)
