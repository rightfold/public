-- |
-- Types.
module Granite.Behavioral.Type
  ( -- * Types
    Unknown (..)
  , Skolem (..)
  , Type (..)

    -- * Translating from AST
  , typeFromExpression

    -- * Constants
  , pattern FunctionType
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Common.Name (Infix (..), Name (..))

--------------------------------------------------------------------------------
-- Types

-- |
-- A unique identifier for an unknown type. Unknown types are generated using
-- type inference, to be unified later.
data Unknown =
  Unknown Word (Maybe Name)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- A unique identifier for a Skolem. A Skolem is a type that is bound by a
-- universal quantifier and therefore completely opaque. Unifies only with
-- itself.
data Skolem =
  Skolem Word (Maybe Name)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- A type. Different from @'Granite.Behavioral.Abstract.Expression' 1@ in that
-- this has no annotations and supports unknown types and Skolems.
data Type
  = UnknownType Unknown
  | SkolemType Skolem
  | VariableType Name
  | ApplicationType Type Type
  | ForallType Name Type
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

--------------------------------------------------------------------------------
-- Translating from AST

-- |
-- Translate an expression in the universe of types into a type.
typeFromExpression :: Expression 1 -> Type
typeFromExpression (Expression _ payload) = case payload of

  VariableExpression name ->
    VariableType name

  ApplicationExpression function argument ->
    let function' = typeFromExpression function in
    let argument' = typeFromExpression argument in
    ApplicationType function' argument'

  ForallExpression parameter body ->
    let body' = typeFromExpression body in
    ForallType parameter body'

--------------------------------------------------------------------------------
-- Constants

pattern FunctionType :: Type -> Type -> Type
pattern FunctionType t1 t2 =
  ApplicationType
    (ApplicationType
      (VariableType (InfixName InfixHyphenGreater))
      t1)
    t2
