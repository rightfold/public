module Granite.Interface
  ( MonadGlobalEnvironment (..)
  ) where

import GHC.TypeLits (type (+))
import Granite.Name (Name)
import Granite.Source (Expression)

-- |
-- Monad for interacting with the global environment.
class Monad f => MonadGlobalEnvironment u f where
  -- |
  -- Look up a definition, returning its type.
  lookupDefinition :: Name -> f (Expression (u + 1))
