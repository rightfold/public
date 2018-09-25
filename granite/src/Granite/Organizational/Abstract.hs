-- |
-- Organizational abstract syntax tree.
module Granite.Organizational.Abstract
  ( Definition (..)
  , DefinitionPayload (..)
  ) where

import Granite.Behavioral.Abstract (Expression)
import Granite.Common.Name (Name)
import Granite.Common.Position (Position)

-- |
-- A definition is a top-level definition. A source file is a list of
-- definitions.
data Definition =
  Definition
    { definitionPosition :: Position
    , definitionPayload  :: DefinitionPayload }
  deriving stock (Eq, Show)

data DefinitionPayload
  -- |
  -- The type and the value are both optional. When the definition is scanned
  -- as an interface, the definition is skipped when the type is absent.
  -- Likewise, when the definition is scanned as an implementation, the
  -- definition is skipped when the value is absent.
  --
  -- The same name may be defined twice, once with only a definition and once
  -- with only an implementation. This allows for the interface and
  -- implementation to be defined in separate source files.
  --
  -- Note that for the program to properly compile and link, if the value is
  -- used then both the type and the value must be given eventually.
  = ValueDefinition Name (Maybe (Expression 1)) (Maybe (Expression 0))
  deriving stock (Eq, Show)
