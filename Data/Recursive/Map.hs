{-# LANGUAGE TypeFamilies #-}
{- | The type 'RMap' @a@ @b@ is like 'M.Map' @a@ @b@, but allows recursive definitions:

>>> :{
  let m1 = RM.insert 23 "Hello" m2
      m2 = RM.insert 42 "World" m1
  in RM.get m1
 :}
fromList [(23,"Hello"),(42,"World")]

All functions in this API are monotone with regard to the ordering of maps that
uses the /discrete/ order on its elements. Furthermore, we only include
functions where the key set does not depend on the actual values of the maps.

This means that maps defined recursively using functions like 'RM.insertWith'
can be used to construct cyclic data structures:

>>> :{
  let m = RM.insertWith (++) 23 "Hi" m
  in take 20 $ RM.get m M.! 23
 :}
"HiHiHiHiHiHiHiHiHiHi"

And because the APIs provided by this package work similar to cyclic data
structures, we can use them inside these maps:

>>> :{
  let m = RM.insertWith RS.union 23 (RS.singleton "Hi") m
  in RM.get m
 :}
fromList [(23,fromList ["Hi"])]

I am looking for a concice but useful example for this feature to be put here! 

An alternative would be to order these maps using a pointwise order on the maps
of elements (and do a simple fixed-point iteration underneath). But then we
could provide a general 'RM.unionWith' function, because not every function
passed to it would be monotone.

-}
module Data.Recursive.Map
  ( RMap
  , get
  , mk
  , empty
  , singleton
  , insert
  , insertWith
  , union
  , unionWith
  , fromSet
  , keysSet
  , restrictKeys
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce
import Data.Monoid
import Control.Monad

import Data.Recursive.Internal
import qualified Data.Recursive.Set as RS

-- $setup
-- >>> :load Data.Recursive.Set Data.Recursive.Map
-- >>> :module - Data.Recursive.Set Data.Recursive.Map
-- >>> import qualified Data.Recursive.Set as RS
-- >>> import qualified Data.Recursive.Map as RM
-- >>> import qualified Data.Set as S
-- >>> import qualified Data.Map as M
-- >>> :set -XFlexibleInstances
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance (Ord a, Arbitrary a) => Arbitrary (RS.RSet a) where arbitrary = RS.mk <$> arbitrary
-- >>> instance (Ord a, Show a) => Show (RS.RSet a) where show = show . RS.get
-- >>> instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (RM.RMap a b) where arbitrary = RM.mk <$> arbitrary
-- >>> instance (Ord a, Show a, Show b) => Show (RM.RMap a b) where show = show . RM.get

-- | Extracts the value of a 'MSet'
get :: RMap a b -> M.Map a b
get (RMap _s m) = m

-- | prop> RM.get (RM.mk m) === m
mk :: M.Map a b -> RMap a b
mk m = RMap (RS.mk (M.keysSet m)) m

-- | prop> RM.get RM.empty === M.empty
empty :: RMap a b
empty = RMap RS.empty M.empty

-- | prop> RM.get (RM.singleton k v) === M.singleton k v
singleton :: a -> b -> RMap a b
singleton k v = RMap (RS.singleton k) (M.singleton k v)

build :: Ord a => RS.RSet a -> M.Map a b -> RMap a b
build s m = RMap s (M.fromSet (m M.!) (RS.get s))


-- | prop> RM.get (RM.insert k v m) === M.insert k v (RM.get m)
insert :: Ord a => a -> b -> RMap a b -> RMap a b
insert k v ~(RMap rs m) = build (RS.insert k rs) (M.insert k v m)

-- | prop> RM.get (RM.union m1 m2) === M.union (RM.get m1) (RM.get m2)
union :: Ord a => RMap a b -> RMap a b -> RMap a b
union ~(RMap rs1 m1) ~(RMap rs2 m2) = build (RS.union rs1 rs2) (M.union m1 m2)

-- | prop> RM.get (RM.insertWith (applyFun2 f) k v m) === M.insertWith (applyFun2 f) k v (RM.get m)
insertWith :: Ord a => (b -> b -> b) -> a -> b -> RMap a b -> RMap a b
insertWith f k v ~(RMap rs m) = build (RS.insert k rs) (M.insertWith f k v m)

-- | prop> RM.get (RM.unionWith (applyFun2 f) m1 m2) === M.unionWith (applyFun2 f) (RM.get m1) (RM.get m2)
unionWith :: Ord a => (b -> b -> b) -> RMap a b -> RMap a b -> RMap a b
unionWith f ~(RMap rs1 m1) ~(RMap rs2 m2) = build (RS.union rs1 rs2) (M.unionWith f m1 m2)

-- | prop> RM.get (RM.singleton k v) === M.singleton k v
fromSet :: (a -> b) -> RS.RSet a -> RMap a b
fromSet f s = RMap s (M.fromSet f (RS.get s))

-- | prop> RS.get (RM.keysSet m) === M.keysSet (RM.get m)
keysSet :: RMap a b -> RS.RSet a
keysSet ~(RMap rs m) = RS.id rs
  -- better use RS.id either here or in fromSet, to avoid unproductive loops


-- | prop> RM.get (RM.restrictKeys m s) === M.restrictKeys (RM.get m) (RS.get s)
restrictKeys :: Ord a => RMap a b -> RS.RSet a -> RMap a b
restrictKeys ~(RMap rs m) s2 =
    build (rs `RS.intersection` s2) (M.restrictKeys m (RS.get s2))
