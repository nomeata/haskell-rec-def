{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module Data.Recursive.FastSet
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map.Merge.Lazy
import Data.Set
import Data.Coerce

import Data.POrder
import Data.Recursive.R.Internal
import Data.Recursive.Propagator.Seminaive
import qualified Data.Recursive.Propagator.Seminaive as Seminaive
import Data.Recursive.Propagator.Class

newtype FastSet a = FastSet { fastSet :: Set a }
  deriving (Show, Eq, Ord)

instance Eq a => POrder (FastSet a)
instance Eq a => Bottom (FastSet a) where bottom = FastSet empty

instance Ord a => ChangeAction (S.Set a) (FastSet a) where
  update = coerce union
  diff = coerce difference
  noop = coerce isSubsetOf

instance Ord a => Change (FastSet a) where type Delta (FastSet a) = Set a

instance Ord a => HasPropagator (FastSet a) where
  type Prop (FastSet a) = Seminaive.Prop (FastSet a)

rEmpty :: Ord a => R (FastSet a)
rEmpty = mkR bottom

rInsert :: Ord a => a -> R (FastSet a) -> R (FastSet a)
-- We could remove x from the delta to a here, but it shouldn't be necessary.
rInsert x = defR1 $ lift1 (coerce $ S.insert x) (S.delete x . delta)

rFilter :: Ord a => (a -> Bool) -> R (FastSet a) -> R (FastSet a)
rFilter f = defR1 $ lift1 (coerce $ S.filter f) (S.filter f . delta)

rUnion :: Ord a => R (FastSet a) -> R (FastSet a) -> R (FastSet a)
rUnion = defR2 $ lift2 (coerce S.union) f1 f2
  where
    f1 ua b = delta ua `S.difference` fastSet b
    f2 a ub = delta ub `S.difference` fastSet a

-- I can find a better way to do this.
rUnions :: Ord a => [R (FastSet a)] -> R (FastSet a)
rUnions = Prelude.foldl rUnion rEmpty

rFromList :: Ord a => [a] -> R (FastSet a)
rFromList = mkR . FastSet . S.fromList


-- Okay, let's try it out!
trans :: forall a. Ord a => [(a, a)] -> M.Map a [a]
trans edges = M.map (S.toList . fastSet . getR) sets
  where
    g = M.fromListWith (<>) $
        [(v2, []) | (_, v2) <- edges] ++
        [(v1, [v2]) | (v1, v2) <- edges]
    sets :: M.Map a (R (FastSet a))
    sets = M.map reachable g
    reachable vs = rUnion (rFromList vs) $ rUnions [ sets M.! v' | v' <- vs ]


-- -- Let's do the same for maps.
-- instance (Eq k, POrder v) => POrder (M.Map k v)
-- -- NB. being mapped to bottom is the same as being absent.
-- instance (Eq k, POrder v) => Bottom (M.Map k v) where bottom = M.empty
-- instance (Ord k, Bottom v, ChangeAction dv v) => ChangeAction (M.Map k dv) (M.Map k v) where
--   update = merge (mapMissing g) (mapMissing $ const id) (zipWithMatched $ const update)
--     where g k da = update da bottom
--   diff y x = 

