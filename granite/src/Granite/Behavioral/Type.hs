-- |
-- Types.
module Granite.Behavioral.Type
  ( Unknown (..)
  , Skolem (..)
  , Type (..)
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Granite.Common.Name (Name)

-- |
-- A unique identifier for an unknown type. Unknown types are generated using
-- type inference, to be unified later.
newtype Unknown =
  Unknown Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

-- |
-- A unique identifier for a Skolem. A Skolem is a type that is bound by a
-- universal quantifier and therefore completely opaque. Unifies only with
-- itself.
newtype Skolem =
  Skolem Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

-- |
-- A type. Different from @'Granite.Behavioral.Abstract.Expression' 1@ in that
-- this has no annotations and supports unknown types and Skolems.
data Type
  = UnknownType Unknown
  | SkolemType Skolem
  | VariableType Name
  | ApplicationType Type Type
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)
