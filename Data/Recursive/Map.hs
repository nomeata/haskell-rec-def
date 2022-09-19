{-# LANGUAGE TypeFamilies #-}
{- | The type 'RMap' @a@ @b@ is like 'M.Map' @a@ @b@, but allows recursive definitions:

>>> :{
  let m1 = RM.insert 23 "Hello" m2
      m2 = RM.insert 42 "World" m1
  in RM.get m1
 :}
fromList [23,42]

-}
module Data.Recursive.Map
  ( RMap
  , mk
  , get
  , empty
  , insert
  , insertWith
  , unionWith

  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce
import Data.Monoid
import Control.Monad

import Data.Recursive.Internal
import qualified Data.Recursive.Set as RS

mk :: M.Map a b -> RMap a b
mk m = RMap (RS.mk (M.keysSet m)) m

get :: RMap a b -> M.Map a b
get (RMap _s m) = m

empty :: RMap a b
empty = RMap RS.empty M.empty

build :: Ord a => RS.RSet a -> M.Map a b -> RMap a b
build s m = RMap s (M.fromSet (m M.!) (RS.get s))

insert :: Ord a => a -> b -> RMap a b -> RMap a b
insert k v ~(RMap rs m) = build (RS.insert k rs) (M.insert k v m)

unionWith :: Ord a => (b -> b -> b) -> RMap a b -> RMap a b -> RMap a b
unionWith f ~(RMap rs1 m1) ~(RMap rs2 m2) = build (RS.union rs1 rs2) (M.unionWith f m1 m2)

insertWith :: Ord a => (b -> b -> b) -> a -> b -> RMap a b -> RMap a b
insertWith f k v ~(RMap rs m) = build (RS.insert k rs) (M.insertWith f k v m)

{-

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (R (M.Map a b)) where arbitrary = mkR <$> arbitrary
-- >>> instance (Eq a, Show a, Eq b, Show b) => Show (R (M.Map a b)) where show = show . getR

-- | prop> getR rEmpty === M.empty
rEmpty :: Eq a => R (M.Map a b)
rEmpty = mkR M.empty

-- | prop> getR (rInsert k v r1) === M.insert k v (getR r1)
rInsert :: Ord a => a -> b -> R (M.Map a b) -> R (M.Map a b)
rInsert k v = defR1 $ lift1 $ M.insert k v

-- | prop> getR (rDelete k r1) === M.delete k (getR r1)
rDelete :: Ord a => a -> R (M.Map a b) -> R (M.Map a b)
rDelete x = defR1 $ lift1 $ M.delete x

-- | prop> \(Fun _ f) -> getR (rUnionWith f r1 r2) === S.unionWith f (getR r1) (getR r2)
rUnionWith :: Ord a => (b -> b -> b) -> R (M.Map a b) -> R (M.Map a b) -> R (M.Map a b)
rUnionWith f = defR2 $ lift2 (M.unionWith f)

-- | prop> getR (rUnion r1 r2) === S.union (getR r1) (getR r2)
rUnion :: Ord a => R (M.Map a b) -> R (M.Map a b) -> R (M.Map a b)
rUnion = defR2 $ lift2 M.union

-- | prop> getR (rUnions rs) === S.unions (map getR rs)
rUnions :: Ord a => [R (M.Map a b)] -> R (M.Map a b)
rUnions = defRList $ liftList M.unions

-- | prop> getR (rIntersection r1 r2) === S.intersection (getR r1) (getR r2)
rIntersection :: Ord a => R (M.Map a b) -> R (M.Map a b) -> R (M.Map a b)
rIntersection = defR2 $ lift2 M.intersection

-- | prop> getR (rMember n r1) === S.member n (getR r1)
rMember :: Ord a => a -> R (M.Map a b) -> R Bool
rMember x = defR1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (M.member x s) $ coerce setTop pb
    watchProp ps update
    update

-- | prop> getRDual (rNotMember n r1) === S.notMember n (getR r1)
rNotMember :: Ord a => a -> R (M.Map a b) -> R (Dual Bool)
rNotMember x = defR1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (M.member x s) $ coerce setTop pb
    watchProp ps update
    update

-- | prop> getRDual (rDisjoint r1 r2) === S.disjoint (getR r1) (getR r2)
rDisjoint :: Ord a => R (M.Map a b) -> R (M.Map a b) -> R (Dual Bool)
rDisjoint = defR2 $ \ps1 ps2 (PDualBool pb) -> do
    let update = do
            s1 <- readProp ps1
            s2 <- readProp ps2
            unless (M.disjoint s1 s2) $ coerce setTop pb
    watchProp ps1 update
    watchProp ps2 update
    update
-}
