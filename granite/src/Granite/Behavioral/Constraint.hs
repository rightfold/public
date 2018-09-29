-- |
-- Constraints are introduced by type inference and solved afterwards to check
-- for any type errors.
module Granite.Behavioral.Constraint
  ( ConstraintSet
  , newConstraintSet
  , insertTypeEquality
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.HashTable.ST.Cuckoo (HashTable)

import qualified Data.HashTable.ST.Cuckoo as HashTable

import Granite.Behavioral.Type (Type)

-- |
-- A set of constraints in some state thread.
data ConstraintSet s =
  ConstraintSet
    { -- Why use cuckoo hashing? The inserts are faster, at the expense of
      -- slower lookups. We only do inserts and traversals, no lookups.
      typeEqualityConstraints :: HashTable s (Type, Type) () }

-- |
-- A new, empty constraint set.
newConstraintSet :: PrimMonad m => m (ConstraintSet (PrimState m))
newConstraintSet = ConstraintSet <$> stToPrim HashTable.new

-- |
-- Insert a type equality constraint into a constraint set.
insertTypeEquality :: PrimMonad m => ConstraintSet (PrimState m)
                   -> Type -> Type -> m ()
insertTypeEquality (ConstraintSet constraints) typeA typeB = stToPrim $
  -- By first sorting the pair of types, there should be fewer distinct
  -- constraints, and constraint solving should be faster.
  let constraint = sortPair (typeA, typeB) in
  HashTable.insert constraints constraint ()

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (a, b) | a < b     = (a, b)
                | otherwise = (b, a)
