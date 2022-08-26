module Data.POrder where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce
import qualified Data.Set as S

-- | This (empty) class indicates that the type 'a' is partially ordered.
-- The class is empty because we do not need any of the operations on runtime.
-- Nevertheless the order better exists for the safety of this API.
--
-- This order may be unrelated to the total order given by 'Ord'.
class Eq a => POrder a

-- | A class indicating that the type 'a' is partially ordered and has a bottom
-- element.
class POrder a => Bottom a where bottom :: a

-- | A class indicating that the type 'a' is partially ordered and has a top
-- element.
class POrder a => Top a where top :: a

instance POrder a => POrder (Dual a)

instance Top a => Bottom (Dual a) where bottom = Dual top

-- Annoyingly, we have to give all instances here, to avoid orphans

-- | Arbitraily using the @False < True@ order
instance POrder Bool
instance Bottom Bool where bottom = False
instance Top Bool where top = True

instance Eq a => POrder (S.Set a)
instance Eq a => Bottom (S.Set a) where bottom = S.empty


