module RParser2 (Parser, parse, parses, sat', sat, token, tok) where

import Data.Unique
import Data.Typeable
import System.IO.Unsafe
import Control.Applicative
import Data.Maybe

import qualified Data.Recursive.Set as RS
import qualified Data.Set as S

import qualified Parser as P

import Debug.Trace

data Parser tok a = MkP
    { unP :: P.Parser Unique tok a
    , refs :: RS.RSet Unique }

lift0 :: P.Parser Unique tok a -> Parser tok a
lift0 p = MkP p RS.empty

mkMemoP r p = withUnique $ \u -> MkP
  (if u `S.member` RS.get r
   then -- traceShow (hashUnique u) $
        P.memoise u p
   else p)
  (RS.insert u r)

lift1 :: (P.Parser Unique tok a -> P.Parser Unique tok b)
       -> (Parser tok a -> Parser tok b)
lift1 pf ~(MkP p1 r1) =
    mkMemoP r1 (pf p1)

lift2 :: (P.Parser Unique tok a -> P.Parser Unique tok b -> P.Parser Unique tok c)
       -> (Parser tok a -> Parser tok b -> Parser tok c)
lift2 pf ~(MkP p1 r1) ~(MkP p2 r2) =
    mkMemoP (r1 `RS.union` r2) (pf p1 p2)

withUnique :: (Unique -> a) -> a
withUnique f = unsafePerformIO $ f <$> newUnique

parses :: Parser tok a -> [tok] -> [a]
parses (MkP p _) = P.parse p

parse :: Parser tok a -> [tok] -> Maybe a
parse p = listToMaybe . parses p

sat' :: Typeable a => (tok -> Maybe a) -> Parser tok a
sat' p = lift0 (P.sat' p)

sat :: Typeable tok => (tok -> Bool) -> Parser tok tok
sat p = lift0 (P.sat p)

token :: Typeable tok => Parser tok tok
token = lift0 P.token

tok :: Eq tok => tok -> Parser tok tok
tok t = lift0 (P.tok t)

instance Functor (Parser tok) where
  fmap f = lift1 (fmap f)

instance Applicative (Parser tok) where
  pure x = lift0 (pure x)
  (<*>) = lift2 (<*>)

{-
instance Monad (Parser tok) where
  return = pure
  p1 >>= f = withMemo $ unP p1 >>= unP . f
-}

instance Alternative (Parser tok) where
  empty = lift0 empty
  (<|>) = lift2 (<|>)

-- The large example from https://okmij.org/ftp/Haskell/LeftRecursion.hs

(>>>) :: Monoid a => Parser tok a -> Parser tok a -> Parser tok a
p1 >>> p2 = liftA2 (<>) p1 p2

char :: Char -> Parser Char String
char a = pure <$> tok a

s,a,b,c :: Parser Char String
s = s >>> a >>> c <|> c
a = b <|> char 'a' >>> c >>> char 'a'
b = id <$> b
c = char 'b' <|> c >>> a

-- ghci> parse s "babababa"
-- ["babababa"]
