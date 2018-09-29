-- |
-- Working with interfaces. Interfaces are mappings from definition names to
-- their types. These are used when inferring types of variable expressions that
-- reference top-level definitions.
module Granite.Organizational.Interface
  ( -- * Interfaces
    Interface (..)

    -- * Interface collection
  , CollectError (..)
  , collectInterface
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Error.Class (throwError)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap

import Granite.Behavioral.Type (Type, typeFromExpression)
import Granite.Common.Name (Name)
import Granite.Common.Position (Position)
import Granite.Organizational.Abstract (Definition (..), DefinitionPayload (..))

-- |
-- Interfaces.
newtype Interface =
  Interface (HashMap Name (Position, Type))
  deriving stock (Eq, Show)

-- |
-- Errors that occur during collection.
data CollectError
  -- |
  -- A value may not be defined with a type twice.
  = ValueRedefinitionError Position Name
  deriving stock (Eq, Show)

-- |
-- Collect the interface of a list of definitions. The definitions may come from
-- multiple different source files.
collectInterface :: Foldable f => f Definition -> Either CollectError Interface
collectInterface = fmap Interface . foldM step HashMap.empty
  where
  step acc (Definition position payload) = case payload of

    ValueDefinition name (Just typeExpr) _ -> do
      when (HashMap.member name acc) $
        throwError $ ValueRedefinitionError position name
      let entry = (position, typeFromExpression typeExpr)
      pure $ HashMap.insert name entry acc

    ValueDefinition _ Nothing _ ->
      pure acc
