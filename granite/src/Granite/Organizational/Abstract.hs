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
  = ValueDefinition Name (Expression 1) (Expression 0)
  deriving stock (Eq, Show)
