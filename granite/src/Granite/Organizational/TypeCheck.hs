-- |
-- Type check an implementation.
module Granite.Organizational.TypeCheck
  ( Error (..)
  , typeCheckImplementation
  , typeCheckDefinition
  ) where

import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))

import qualified Data.HashMap.Strict as HashMap

import Granite.Behavioral.TypeCheck (typeCheckExpression)
import Granite.Common.Name (Name)
import Granite.Organizational.Abstract (Definition (..), DefinitionPayload (..))
import Granite.Organizational.Interface (Interface (..))

import qualified Granite.Behavioral.TypeCheck as Behavioral.TypeCheck

data Error
  = BehavioralError Behavioral.TypeCheck.Error
  | MissingInterface Name
  deriving stock (Eq, Show)

typeCheckImplementation :: Foldable f => Interface -> f Definition -> Either Error ()
typeCheckImplementation = traverse_ . typeCheckDefinition

typeCheckDefinition :: Interface -> Definition -> Either Error ()
typeCheckDefinition ifc@(Interface ifcValues) (Definition _ payload) =
  case payload of
    ValueDefinition name _ (Just bodyExpr) ->
      case HashMap.lookup name ifcValues of
        Nothing      -> throwError (MissingInterface name)
        Just (_, ty) -> typeCheckExpression ifc (Just ty) bodyExpr
                          & first BehavioralError

    ValueDefinition _ _ Nothing ->
      pure ()
