{-# LANGUAGE TemplateHaskell #-}

-- |
-- Infer types of expressions.
module Granite.Behavioral.Infer
  ( -- * Infrastructure
    Error (..)
  , Environment (..)
  , State
  , freshUnknown
  , freshSkolem
  , runInfer

    -- * Inference
  , infer
  ) where

import Control.Lens ((^.), (?~), (%=), (<<%=), at, makeLenses, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Data.HashMap.Strict (HashMap)

import qualified Control.Monad.Reader.Class as Reader

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Behavioral.Constraint (ConstraintSet)
import Granite.Behavioral.Type (Skolem (..), Type (..), Unknown (..))
import Granite.Common.Name (Infix (..), Name (..))
import Granite.Common.Position (Position)

import qualified Granite.Behavioral.Constraint as ConstraintSet

-- |
-- Error that occurs during inference.
data Error
  = UnknownName Position Name
  deriving stock (Eq, Show)

-- |
-- Environment in which to infer types.
newtype Environment = Environment { _envVariables :: HashMap Name Type }
$(makeLenses ''Environment)

-- |
-- State used while inferring types.
data State =
  State
    { _stateConstraints :: ConstraintSet
    , _stateNextFresh :: Word }
$(makeLenses ''State)

-- |
-- Generate a fresh unknown.
freshUnknown :: MonadState State m => m Unknown
freshUnknown = Unknown <$> (stateNextFresh <<%= succ)

-- |
-- Generate a fresh Skolem.
freshSkolem :: MonadState State m => m Skolem
freshSkolem = Skolem <$> (stateNextFresh <<%= succ)

-- |
-- Run an inference action.
runInfer :: RWST Environment () State (Either Error) a
         -> Environment
         -> Either Error (ConstraintSet, a)
runInfer action env =
  extract <$> runRWST action env (State ConstraintSet.empty 0)
  where extract (a, s, ()) = (s ^. stateConstraints, a)

-- |
-- Infer the type of an expression, collecting constraints.
infer :: ( MonadError Error m
         , MonadReader Environment m
         , MonadState State m )
      => Expression 0 -> m Type
infer (Expression position payload) = case payload of

  VariableExpression name ->
    maybe (throwError (UnknownName position name)) pure =<<
      view (envVariables . at name)

  ApplicationExpression function argument -> do
    functionType <- infer function
    argumentType <- infer argument

    parameterType <- UnknownType <$> freshUnknown
    returnType <- UnknownType <$> freshUnknown

    insertTypeEquality functionType (makeFunctionType parameterType returnType)
    insertTypeEquality argumentType parameterType

    pure returnType

  LambdaExpression parameter body -> do
    parameterType <- UnknownType <$> freshUnknown
    let localize = envVariables . at parameter ?~ parameterType
    returnType <- Reader.local localize $ infer body
    pure $ makeFunctionType parameterType returnType

makeFunctionType :: Type -> Type -> Type
makeFunctionType parameterType returnType =
  ApplicationType (ApplicationType (VariableType arrow) parameterType) returnType
  where arrow = InfixName InfixHyphenGreater

insertTypeEquality :: MonadState State m => Type -> Type -> m ()
insertTypeEquality typeA typeB =
  stateConstraints %= ConstraintSet.insertTypeEquality typeA typeB
