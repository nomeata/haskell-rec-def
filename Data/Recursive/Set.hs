{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.Set
  ( R
  , r
  , getR
  , module Data.Recursive.Set
  ) where

import qualified Data.Set as S
import Data.Coerce
import Data.Monoid
import Control.Monad

import Data.Recursive.R.Internal
import Data.Recursive.Propagator.Naive
import Data.Recursive.Propagator.P2

rEmpty :: Eq a => R (S.Set a)
rEmpty = r S.empty

rInsert :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rInsert x = defR1 $ lift1 $ S.insert x

rDelete :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rDelete x = defR1 $ lift1 $ S.delete x

rFilter :: Ord a => (a -> Bool) -> R (S.Set a) -> R (S.Set a)
rFilter f = defR1 $ lift1 $ S.filter f

rUnion :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rUnion = defR2 $ lift2 S.union

rUnions :: Ord a => [R (S.Set a)] -> R (S.Set a)
rUnions = defRList $ liftList S.unions

rIntersection :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rIntersection = defR2 $ lift2 S.intersection

rMember :: Ord a => a -> R (S.Set a) -> R Bool
rMember x = defR1 $ \ps pb -> watchProp ps $ do
    let update = do
            s <- readProp ps
            when (S.member x s) $ coerce setTop pb
    watchProp ps update
    update

rNotMember :: Ord a => a -> R (S.Set a) -> R (Dual Bool)
rNotMember x = defR1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (S.notMember x s) $ coerce setTop pb
    watchProp ps update
    update

rDisjoint :: Ord a => R (S.Set a) -> R (S.Set a) -> R (Dual Bool)
rDisjoint = defR2 $ \ps1 ps2 (PDualBool pb) -> do
    let update = do
            s1 <- readProp ps1
            s2 <- readProp ps2
            when (S.disjoint s1 s2) $ coerce setTop pb
    watchProp ps1 update
    watchProp ps2 update
    update
