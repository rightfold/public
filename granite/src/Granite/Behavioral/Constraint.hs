{-# LANGUAGE TemplateHaskell #-}

-- |
-- Constraints are introduced by type inference and solved afterwards to check
-- for any type errors.
module Granite.Behavioral.Constraint
  ( -- * Constraint sets
    ConstraintSet
  , empty
  , insertTypeEquality

    -- * Solving constraints
  , SolveError (..)
  , solve
  ) where

import Control.Lens ((?~), at, makeLenses)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)

import qualified Data.HashSet as HashSet

import Granite.Behavioral.Type (Type)
import Granite.Behavioral.Unify (runUnify, unify)

import qualified Granite.Behavioral.Unify as Unify

--------------------------------------------------------------------------------
-- Constraint sets

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

--------------------------------------------------------------------------------
-- Solving constraints

-- |
-- Error during solving.
data SolveError
  = UnifyError Unify.Error
  deriving stock (Eq, Show)

-- |
-- Solve a constraint set.
solve :: ConstraintSet -> Either SolveError ()
solve (ConstraintSet constraints) =
  first UnifyError . runUnify $
    traverse_ (uncurry unify) $
      constraints
