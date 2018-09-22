module Granite.Source
  ( Definition (..)
  , Expression (..)
  ) where

import GHC.TypeLits (Nat)

import Granite.Name (Name)

-- |
-- A definition is a top-level definition. A source file is a list of
-- definitions.
data Definition
  = ValueDefinition Name (Expression 1) (Expression 0)
  deriving stock (Eq, Show)

-- |
-- The 'Expression' type is used for expressions at any universe: values,
-- types, and kinds. It is a type constructor indexed by the universe. It is a
-- GADT because not all expressions can occur at all universes.
--
-- Using a single GADT for expressions at different universes makes it easy to
-- reuse logic that is common to all of these universes, such as type inference
-- and pretty printing.
data Expression :: Nat -> * where
  VariableExpression :: Name -> Expression u
  ApplicationExpression :: Expression u -> Expression u -> Expression u
  LambdaExpression :: Name -> Expression 0 -> Expression 0
deriving stock instance Eq (Expression u)
deriving stock instance Show (Expression u)
