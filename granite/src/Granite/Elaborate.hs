module Granite.Elaborate
  ( MonadGlobalEnvironment (..)
  , MonadEnvironment
  , elaborate
  ) where

import GHC.TypeLits (type (+))
import Granite.Interface (MonadGlobalEnvironment, lookupDefinition)
import Granite.Name (Name)

import qualified Granite.Source as S

-- |
-- Monad for interacting with a local environment.
class Monad f => MonadLocalEnvironment u f where
  lookupVariable :: Name -> f (F.Expression (u + 1))

-- |
-- Monad for interacting with an environment.
type MonadEnvironment u f = (MonadGlobalEnvironment u f, MonadLocalEnvironment u f)

-- |
-- Convert an expression from source form into System F form.
elaborate :: MonadEnvironment u f => S.Expression u -> f (F.Expression u)

elaborate (S.VariableExpression variable) = do
  tybe <- lookupVariable variable
  _

elaborate (S.ApplicationExpression function argument) = _

elaborate (S.LambdaExpression parameter body) = _
