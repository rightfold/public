module Granite.Common.Name
  ( Name (..)
  , Infix (..)
  , Prefix (..)
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
  | PrefixName Prefix
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- An infix operator.
data Infix :: * where
  InfixAsterisk :: Infix
  InfixHyphen :: Infix
  InfixHyphenGreater :: Infix
  InfixPlus :: Infix
  InfixSolidus :: Infix
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

-- |
-- A prefix operator.
data Prefix :: * where
  PrefixAsterisk :: Prefix
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)
