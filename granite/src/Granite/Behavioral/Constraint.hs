{-# LANGUAGE TemplateHaskell #-}

-- |
-- Constraints are introduced by type inference and solved afterwards to check
-- for any type errors.
module Granite.Behavioral.Constraint
  ( ConstraintSet
  , empty
  , insertTypeEquality
  ) where

import Control.Lens ((?~), at, makeLenses)
import Data.HashSet (HashSet)

import qualified Data.HashSet as HashSet

import Granite.Behavioral.Type (Type)

-- |
-- A set of constraints.
data ConstraintSet =
  ConstraintSet
    { _typeEqualityConstraints :: HashSet (Type, Type) }
$(makeLenses ''ConstraintSet)

-- |
-- An empty constraint set.
empty :: ConstraintSet
empty = ConstraintSet HashSet.empty

-- |
-- Insert a type equality constraint into a constraint set.
insertTypeEquality :: Type -> Type -> ConstraintSet -> ConstraintSet
insertTypeEquality typeA typeB =
  -- By first sorting the pair of types, there should be fewer distinct
  -- constraints, and constraint solving should be faster.
  let constraint = sortPair (typeA, typeB) in
  typeEqualityConstraints . at constraint ?~ ()

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (a, b) | a < b     = (a, b)
                | otherwise = (b, a)
