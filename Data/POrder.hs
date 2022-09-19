-- | This module provides the 'POrder' and related classes
module Data.POrder where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce
import qualified Data.Set as S
import Numeric.Natural
import Data.Function

-- | This class indicates that the type @a@ is partially ordered by some relation ⊑.
--
-- The class does not actually have a method for ⊑, because we do not need it at runtime.
-- Nevertheless the order better exists for the safety of this API.
--
-- This order may be unrelated to the total order given by 'Ord'.
class POrder a where
    -- | The `eqOfLe` method checks _related_ elements for equality.
    --
    -- Formally: For all @x ⊑ y@, @eqOfLe x y == True@ iff @x == y@.
    --
    -- This can be more efficient than testing for equality. For example for
    -- sets, '(==)' needs to compare the elements, but @eqOfLe@ only needs to
    -- compare sizes. It is always ok to use '(==)' here.
    eqOfLe :: a -> a -> Bool

-- | A class indicating that the type @a@ is has a bottom
-- element.
class Bottom a where bottom :: a

-- | A class indicating that the type @a@ is has a top
-- element.
class POrder a => Top a where top :: a

-- | The dual order
instance POrder a => POrder (Dual a) where
    eqOfLe (Dual x) (Dual y) = eqOfLe y x

-- | Bottom is the 'top' of @a@
instance Top a => Bottom (Dual a) where bottom = Dual top

-- Annoyingly, we have to give all instances here, to avoid orphans

-- | Arbitrary using the @False < True@ order
instance POrder Bool where eqOfLe = (==)

-- | Bottom is 'False'
instance Bottom Bool where bottom = False

-- | Top is 'True'
instance Top Bool where top = True

-- | Ordered by 'S.subsetOf'
instance POrder (S.Set a) where eqOfLe = (==) `on` S.size

-- | Bottom is 'S.empty'
instance Bottom (S.Set a) where bottom = S.empty

-- | Ordered by '(<=)f'
instance POrder Natural where eqOfLe = (==)

-- | Bottom is 0
instance Bottom Natural where bottom = 0

-- | Adds 'Nothing' as a least element to an existing partial order
instance POrder a => POrder (Maybe a) where
    eqOfLe Nothing Nothing = True
    eqOfLe Nothing (Just _) = False
    eqOfLe (Just x) (Just y) = eqOfLe x y
    eqOfLe (Just _) Nothing = error "eqOfLe/Maybe used with unrelated arguments"

-- | Bottom is 'Nothing'
instance POrder a => Bottom (Maybe a) where bottom = Nothing
