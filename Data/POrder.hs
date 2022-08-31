-- | This module provides the 'POrder' and related classes
module Data.POrder where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce
import qualified Data.Set as S
import Numeric.Natural

-- | This (empty) class indicates that the type @a@ is partially ordered.
-- The class is empty because we do not need any of the operations on runtime.
-- Nevertheless the order better exists for the safety of this API.
--
-- This order may be unrelated to the total order given by 'Ord'.
class Eq a => POrder a

-- | A class indicating that the type @a@ is partially ordered and has a bottom
-- element.
class POrder a => Bottom a where bottom :: a

-- | A class indicating that the type @a@ is partially ordered and has a top
-- element.
class POrder a => Top a where top :: a

-- | The dual order
instance POrder a => POrder (Dual a)

-- | Bottom is the 'top' of @a@
instance Top a => Bottom (Dual a) where bottom = Dual top

-- Annoyingly, we have to give all instances here, to avoid orphans

-- | Arbitrary using the @False < True@ order
instance POrder Bool

-- | Bottom is 'False'
instance Bottom Bool where bottom = False

-- | Top is 'True'
instance Top Bool where top = True

-- | Ordered by 'S.subsetOf'
instance Eq a => POrder (S.Set a)

-- | Bottom is 'S.empty'
instance Eq a => Bottom (S.Set a) where bottom = S.empty

-- | Ordered by '(<=)f'
instance POrder Natural

-- | Bottom is 0
instance Bottom Natural where bottom = 0

-- | Adds 'Nothing' as a least element to an existing partial order
instance POrder a => POrder (Maybe a)

-- | Bottom is 'Nothing'
instance POrder a => Bottom (Maybe a) where bottom = Nothing
