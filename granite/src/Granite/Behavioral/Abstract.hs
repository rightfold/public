-- |
-- Behavioral abstract syntax tree.
module Granite.Behavioral.Abstract
  ( -- * Universes
    Universe (..)
  , pattern UniverseValues
  , pattern UniverseTypes
  , pattern UniverseKinds
  , pattern UniverseSorts

    -- * Expressions
  , Expression (..)
  , ExpressionPayload (..)
  ) where

import GHC.TypeLits (type (+), Nat)

import Granite.Common.Name (Name)
import Granite.Common.Position (Position)

-- |
-- Universes, to be matched up at the type level.
data Universe :: Nat -> * where
  UniverseZero :: Universe 0
  UniverseSucc :: Universe n -> Universe (n + 1)

pattern UniverseValues :: Universe 0
pattern UniverseTypes  :: Universe 1
pattern UniverseKinds  :: Universe 2
pattern UniverseSorts  :: Universe 3
pattern UniverseValues = UniverseZero
pattern UniverseTypes  = UniverseSucc UniverseZero
pattern UniverseKinds  = UniverseSucc (UniverseSucc UniverseZero)
pattern UniverseSorts  = UniverseSucc (UniverseSucc (UniverseSucc UniverseZero))

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
  ApplicationExpression :: Expression u -> Expression u -> ExpressionPayload u
  LambdaExpression :: Name -> Expression 0 -> ExpressionPayload 0
deriving stock instance Eq (ExpressionPayload u)
deriving stock instance Show (ExpressionPayload u)
