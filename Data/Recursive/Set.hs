{-# LANGUAGE TypeFamilies #-}
{- | The type @RS.RSet a@ is like 'S.Set' @a@, but allows recursive definitions:

>>> :{
  let s1 = RS.insert 23 s2
      s2 = RS.insert 42 s1
  in RS.get s1
 :}
fromList [23,42]

-}
module Data.Recursive.Set (RSet, module Data.Recursive.Set) where

import qualified Data.Set as S
import Data.Coerce
import Control.Monad

import Data.Recursive.Internal
import qualified Data.Propagator.Purify as Purify
import Data.Propagator.Naive
import Data.Propagator.P2

-- $setup
-- >>> :load Data.Recursive.Set Data.Recursive.Bool Data.Recursive.DualBool
-- >>> :module - Data.Recursive.Set Data.Recursive.Bool Data.Recursive.DualBool
-- >>> import qualified Data.Recursive.Set as RS
-- >>> import qualified Data.Recursive.Bool as RB
-- >>> import qualified Data.Recursive.DualBool as RDB
-- >>> import qualified Data.Set as S
-- >>> :set -XFlexibleInstances
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance (Ord a, Arbitrary a) => Arbitrary (RS.RSet a) where arbitrary = RS.mk <$> arbitrary
-- >>> instance (Ord a, Show a) => Show (RS.RSet a) where show = show . RS.get

-- | Extracts the value of a 'RSet a'
get :: RSet a -> S.Set a
get (RSet p) = Purify.get p

-- | prop> RB.get (RB.mk s) === s
mk :: S.Set a -> RSet a
mk s = RSet $ Purify.mk s

-- | prop> RS.get RS.empty === S.empty
empty :: RSet a
empty = RSet $ Purify.mk S.empty

-- | prop> RS.get (RS.singleton x) === S.singleton x
singleton :: a -> RSet a
singleton x = RSet $ Purify.mk $ S.singleton x

-- | prop> RS.get (RS.insert n r1) === S.insert n (RS.get r1)
insert :: Ord a => a -> RSet a -> RSet a
insert x = coerce $ Purify.def1 $ lift1 $ S.insert x

-- | prop> RS.get (RS.delete n r1) === S.delete n (RS.get r1)
delete :: Ord a => a -> RSet a -> RSet a
delete x = coerce $ Purify.def1 $ lift1 $ S.delete x

-- | prop> \(Fun _ p) -> RS.get (RS.filter p r1) === S.filter p (RS.get r1)
filter :: Ord a => (a -> Bool) -> RSet a -> RSet a
filter f = coerce $ Purify.def1 $ lift1 $ S.filter f

-- | prop> RS.get (RS.union r1 r2) === S.union (RS.get r1) (RS.get r2)
union :: Ord a => RSet a -> RSet a -> RSet a
union = coerce $ Purify.def2 $ lift2 S.union

-- | prop> RS.get (RS.unions rs) === S.unions (map RS.get rs)
unions :: Ord a => [RSet a] -> RSet a
unions = coerce $ Purify.defList $ liftList S.unions

-- | prop> RS.get (RS.intersection r1 r2) === S.intersection (RS.get r1) (RS.get r2)
intersection :: Ord a => RSet a -> RSet a -> RSet a
intersection = coerce $ Purify.def2 $ lift2 S.intersection

-- | prop> RB.get (RS.member n r1) === S.member n (RS.get r1)
member :: Ord a => a -> RSet a -> RBool
member x = coerce $ Purify.def1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (S.member x s) $ setTop pb
    watchProp ps update
    update

-- | prop> RDB.get (RS.notMember n r1) === S.notMember n (RS.get r1)
notMember :: Ord a => a -> RSet a -> RDualBool
notMember x = coerce $ Purify.def1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (S.member x s) $ setTop pb
    watchProp ps update
    update

-- | prop> RDB.get (RS.null s) === S.null (RS.get s)
null :: RSet a -> RDualBool
null = coerce $ Purify.def1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            unless (S.null s) $ setTop pb
    watchProp ps update
    update

-- | prop> RDB.get (RS.disjoint r1 r2) === S.disjoint (RS.get r1) (RS.get r2)
disjoint :: Ord a => RSet a -> RSet a -> RDualBool
disjoint = coerce $ Purify.def2 $ \ps1 ps2 pb -> do
    let update = do
            s1 <- readProp ps1
            s2 <- readProp ps2
            unless (S.disjoint s1 s2) $ setTop pb
    watchProp ps1 update
    watchProp ps2 update
    update

-- | The identity function. This is useful when tying the knot, to avoid a loop that bottoms out:
--
-- > let x = x in RS.get x
--
-- will not work, but
--
-- >>> let x = RS.id x in RS.get x
-- fromList []
--
-- does.
--
-- | prop> RS.get (RS.id s) === RS.get s
id :: RSet a -> RSet a
id = coerce $ Purify.def1 $ lift1 (Prelude.id :: S.Set a -> S.Set a)
