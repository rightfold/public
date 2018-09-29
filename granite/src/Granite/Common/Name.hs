module Granite.Common.Name
  ( Name (..)
  , Infix (..)
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- |
-- Data type for names of values and types. These correspond to identifiers in
-- the source code when defining or referencing values or types.
--
-- There are no restrictions on names; they can be any arbitrary text.
data Name
  = PlainName Text
  | InfixName Infix
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- An infix operator.
data Infix
  = InfixHyphenGreater
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)
