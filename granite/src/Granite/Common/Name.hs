module Granite.Common.Name
  ( Name (..)
  ) where

import Data.Text (Text)

-- |
-- Data type for names of values and types. These correspond to identifiers in
-- the source code when defining or referencing values or types.
--
-- There are no restrictions on names; they can be any arbitrary text.
newtype Name = Name Text
  deriving stock (Eq, Ord, Show)
