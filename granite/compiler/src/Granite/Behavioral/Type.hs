-- |
-- Types.
module Granite.Behavioral.Type
  ( -- * Types
    Unknown (..)
  , Skolem (..)
  , Type (..)

    -- * Manipulation
  , typeFromExpression
  , dissectFunctionType

    -- * Constants
  , pattern U8Type
  , pattern U64Type
  , pattern F64Type
  , pattern FunctionType
  , pattern EffType
  , pattern PointerType
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Common.Name (Infix (..), Name (..), Prefix (..))

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
-- Manipulation

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

-- |
-- Take a type, and return its parameter types and return type. The list of
-- parameter types may be empty; namely if this is not a function type.
dissectFunctionType :: Type -> ([Type], Type)
dissectFunctionType (FunctionType first ret) =
  let (rest, ret') = dissectFunctionType ret in
  (first : rest, ret')
dissectFunctionType t = ([], t)

--------------------------------------------------------------------------------
-- Constants

pattern U8Type :: Type
pattern U8Type = VariableType (PlainName "u8")

pattern U64Type :: Type
pattern U64Type = VariableType (PlainName "u64")

pattern F64Type :: Type
pattern F64Type = VariableType (PlainName "f64")

pattern FunctionType :: Type -> Type -> Type
pattern FunctionType t1 t2 =
  ApplicationType
    (ApplicationType
      (VariableType (InfixName InfixHyphenGreater))
      t1)
    t2

pattern EffType :: Type -> Type -> Type
pattern EffType t1 t2 =
  ApplicationType
    (ApplicationType
      (VariableType (PlainName "eff"))
      t1)
    t2

pattern PointerType :: Type -> Type
pattern PointerType t =
  ApplicationType (VariableType (PrefixName PrefixAsterisk))
                  t
