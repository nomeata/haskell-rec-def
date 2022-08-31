-- |
-- This module re-exports the safe parts of "Data.Recursive.R.Internal".
--
-- If you import a module like "Data.Recursive.Bool" you do not need to import
-- this module here directly.
module Data.Recursive.R (R, mkR, getR, getRDual) where

import Data.Recursive.R.Internal
