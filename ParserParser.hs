module ParserParser where

import Control.Applicative
import Data.Foldable
import Data.Functor
import Data.Char
import qualified Data.Map as M

import RParser

-- A type for simple grammars

type Ident = String
type RuleRhs = [Seq]
type Seq = [Atom]
data Atom = Lit String | NonTerm Ident deriving Show
type Rule = (Ident, RuleRhs)
type BNF = [Rule]

-- a parser for simple BNF syntax

type P = Parser Char

l :: P a -> P a
l p = p' where -- NB: Sharing!
  p' = p <|> p' <* sat isSpace
quote :: P Char
quote = tok '\''
quoted :: P a -> P a
quoted p = quote *> p <* quote
str :: P String
str = some (sat (not . (== '\'')))
ident :: P Ident
ident = some (sat (\c -> isAlphaNum c && isAscii c))
atom :: P Atom
atom = Lit     <$> l (quoted str)
   <|> NonTerm <$> l ident
eps :: P ()
eps = void $ l (tok 'ε')
sep :: P ()
sep = void $ some (sat isSpace)
sq :: P Seq
sq = []   <$ eps
 <|> (:)  <$> atom <* sep <*> sq
 <|> pure <$> atom
ruleRhs :: P RuleRhs
ruleRhs = pure <$> sq <* l (tok ';')
      <|> (:)  <$> sq <* l (tok '|') <*> ruleRhs
rule :: P Rule
rule = (,) <$> l ident <* l (tok ':' *> tok '=') <*> ruleRhs
bnf :: P BNF
bnf = liftA2 (:) rule bnf <|> pure []

-- Interpreting a BNF, producing a parse tree, i.e. noting which
-- non-terminal fired

interp :: BNF -> P String
interp bnf = parsers M.! start
  where
    start :: Ident
    start = fst (head bnf)

    parsers :: M.Map Ident (P String)
    parsers = M.fromList [ (i, parseRule i rhs) | (i, rhs) <- bnf ]

    parseRule :: Ident -> RuleRhs -> P String
    parseRule ident rhs = trace <$> asum (map parseSeq rhs)
      where trace s = ident ++ "(" ++ s ++ ")"

    parseSeq :: Seq -> P String
    parseSeq = fmap concat . traverse parseAtom

    parseAtom :: Atom -> P String
    parseAtom (Lit s) = traverse tok s
    parseAtom (NonTerm i) = parsers M.! i

-- An example

numExp :: String
numExp = unlines
    [ "term   := sum;"
    , "pdigit := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';"
    , "digit  := '0' | pdigit;"
    , "pnum   := pdigit | pnum digit;"
    , "num    := '0' | pnum;"
    , "prod   := atom | atom '*' prod;"
    , "sum    := prod | prod '+' sum;"
    , "atom   := num | '(' term ')';"
    ]
