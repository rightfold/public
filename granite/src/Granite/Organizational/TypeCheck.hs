-- |
-- Type check an implementation.
module Granite.Organizational.TypeCheck
  ( Error (..)
  , typeCheckImplementation
  , typeCheckDefinition
  ) where

import Data.Foldable (traverse_)

import Granite.Behavioral.TypeCheck (Error (..), typeCheckExpression)
import Granite.Organizational.Abstract (Definition (..), DefinitionPayload (..))
import Granite.Organizational.Interface (Interface)

typeCheckImplementation :: Foldable f => Interface -> f Definition -> Either Error ()
typeCheckImplementation = traverse_ . typeCheckDefinition

typeCheckDefinition :: Interface -> Definition -> Either Error ()
typeCheckDefinition ifc (Definition _ payload) = case payload of
  ValueDefinition _ _ bodyExpr ->
    traverse_ (typeCheckExpression ifc) bodyExpr
