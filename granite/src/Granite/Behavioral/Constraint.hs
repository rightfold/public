-- |
-- Constraints are introduced by type inference and solved afterwards to check
-- for any type errors.
module Granite.Behavioral.Constraint
  ( ConstraintSet
  , insertTypeEquality
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.HashTable.ST.Cuckoo as HashTable

import Granite.Behavioral.Type (Type)

-- |
-- A set of constraints in some state thread.
data ConstraintSet s =
  ConstraintSet (HashTable s Constraint ())

data Constraint
  = TypeEquality Type Type
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- Insert a type equality constraint into a constraint set.
insertTypeEquality :: PrimMonad m => ConstraintSet (PrimState m)
                   -> Type -> Type -> m ()
insertTypeEquality (ConstraintSet constraints) typeA typeB = stToPrim $
  -- By first sorting the pair of types, there should be fewer distinct
  -- constraints, and constraint solving should be faster.
  let (typeA', typeB') = sortPair (typeA, typeB) in
  let constraint = TypeEquality typeA' typeB' in
  HashTable.insert constraints constraint ()

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (a, b) | a < b     = (a, b)
                | otherwise = (b, a)
