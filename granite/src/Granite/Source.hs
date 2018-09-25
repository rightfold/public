module Granite.Source
  ( Definition (..), DefinitionPayload (..)
  , Expression (..), ExpressionPayload (..)
  ) where

import GHC.TypeLits (Nat)

import Granite.Name (Name)
import Granite.Position (Position)

-- |
-- A definition is a top-level definition. A source file is a list of
-- definitions.
data Definition =
  Definition
    { definitionPosition :: Position
    , definitionPayload  :: DefinitionPayload }
  deriving stock (Eq, Show)

data DefinitionPayload
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
data Expression u =
  Expression
    { expressionPosition :: Position
    , expressionPayload  :: ExpressionPayload u }
  deriving stock (Eq, Show)

data ExpressionPayload :: Nat -> * where
  VariableExpression :: Name -> ExpressionPayload u
  ApplicationExpression :: ExpressionPayload u -> ExpressionPayload u -> ExpressionPayload u
  LambdaExpression :: Name -> ExpressionPayload 0 -> ExpressionPayload 0
deriving stock instance Eq (ExpressionPayload u)
deriving stock instance Show (ExpressionPayload u)
