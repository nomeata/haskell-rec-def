module RParser (Parser, parse, sat', sat, token, tok) where

import Data.Unique
import Data.Typeable
import System.IO.Unsafe
import Control.Applicative

import qualified Parser as P

newtype Parser tok a = MkP (P.Parser Unique tok a)

withMemo :: P.Parser Unique tok a -> Parser tok a
withMemo p = unsafePerformIO $ do
    u <- newUnique
    pure $ MkP $ P.memoise u p

parse :: Parser tok a -> [tok] -> [a]
parse (MkP p) = P.parse p

sat' :: Typeable a => (tok -> Maybe a) -> Parser tok a
sat' p = MkP (P.sat' p)

sat :: Typeable tok => (tok -> Bool) -> Parser tok tok
sat p = MkP (P.sat p)

token :: Typeable tok => Parser tok tok
token = MkP P.token

tok :: Eq tok => tok -> Parser tok tok
tok t = MkP (P.tok t)

instance Functor (Parser tok) where
  fmap f (MkP p) = withMemo (fmap f p)

instance Applicative (Parser tok) where
  pure x = MkP (pure x)
  MkP p1 <*> MkP p2 = withMemo (p1 <*> p2)

instance Alternative (Parser tok) where
  empty = MkP empty
  MkP p1 <|> MkP p2 = withMemo (p1 <|> p2)

