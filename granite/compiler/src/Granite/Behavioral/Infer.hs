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
  , assert

    -- * Polymorphism
  , instantiate
  , skolemize
  ) where

import Control.Lens ((&), (^.), (?~), (%=), (<<%=), at, makeLenses, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Data.HashMap.Strict (HashMap)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.HashMap.Strict as HashMap

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Behavioral.Constraint (ConstraintSet)
import Granite.Behavioral.Type (Skolem (..), Type (..), Unknown (..))
import Granite.Common.Name (Infix (..), Name (..))
import Granite.Common.Position (Position)

import qualified Granite.Behavioral.Constraint as ConstraintSet

--------------------------------------------------------------------------------
-- Infrastructure

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
freshUnknown :: MonadState State m => Maybe Name -> m Unknown
freshUnknown hint = Unknown <$> (stateNextFresh <<%= succ) <*> pure hint

-- |
-- Generate a fresh Skolem.
freshSkolem :: MonadState State m => Maybe Name -> m Skolem
freshSkolem hint = Skolem <$> (stateNextFresh <<%= succ) <*> pure hint

-- |
-- Run an inference action.
runInfer :: RWST Environment () State (Either Error) a
         -> Environment
         -> Either Error (ConstraintSet, a)
runInfer action env =
  extract <$> runRWST action env (State ConstraintSet.empty 0)
  where extract (a, s, ()) = (s ^. stateConstraints, a)

--------------------------------------------------------------------------------
-- Inference

-- |
-- Infer the type of an expression, collecting constraints.
infer :: ( MonadError Error m
         , MonadReader Environment m
         , MonadState State m )
      => Expression 0 -> m Type
infer (Expression position payload) = case payload of

  VariableExpression name ->
    maybe (throwError (UnknownName position name))
          instantiate
          =<< view (envVariables . at name)

  ApplicationExpression function argument -> do
    functionType <- infer function
    argumentType <- infer argument

    parameterType <- UnknownType <$> freshUnknown Nothing
    returnType <- UnknownType <$> freshUnknown Nothing

    insertTypeEquality functionType (makeFunctionType parameterType returnType)
    insertTypeEquality argumentType parameterType

    pure returnType

  LambdaExpression parameter body -> do
    parameterType <- UnknownType <$> freshUnknown (Just parameter)
    let localize = envVariables . at parameter ?~ parameterType
    returnType <- Reader.local localize $ infer body
    pure $ makeFunctionType parameterType returnType

  ForeignExpression _ ->
    UnknownType <$> freshUnknown Nothing

-- |
-- Assert that an expression has a given type.
assert :: ( MonadError Error m
          , MonadReader Environment m
          , MonadState State m )
       => Type -> Expression 0 -> m ()
assert expected expression = do
  expected' <- skolemize expected
  actual <- infer expression
  insertTypeEquality expected' actual

--------------------------------------------------------------------------------
-- Polymorphism

-- |
-- Replace variables bound by forall with fresh unknowns.
instantiate :: MonadState State m => Type -> m Type
instantiate = refresh (fmap UnknownType . freshUnknown)

-- |
-- Replace variables bound by forall with fresh Skolems.
skolemize :: MonadState State m => Type -> m Type
skolemize = refresh (fmap SkolemType . freshSkolem)

refresh :: forall m. MonadState State m => (Maybe Name -> m Type) -> Type -> m Type
refresh fresh = go HashMap.empty
  where
  go :: HashMap Name Type -> Type -> m Type
  go _ t@(UnknownType _)     = pure t
  go _ t@(SkolemType _)      = pure t
  go s t@(VariableType name) = pure (HashMap.lookupDefault t name s)
  go s (ApplicationType function argument) = do
    function' <- go s function
    argument' <- go s argument
    pure $ ApplicationType function' argument'
  go s (ForallType parameter body) = do
    t <- fresh (Just parameter)
    go (s & at parameter ?~ t) body

--------------------------------------------------------------------------------

makeFunctionType :: Type -> Type -> Type
makeFunctionType parameterType returnType =
  ApplicationType (ApplicationType (VariableType arrow) parameterType) returnType
  where arrow = InfixName InfixHyphenGreater

insertTypeEquality :: MonadState State m => Type -> Type -> m ()
insertTypeEquality typeA typeB =
  stateConstraints %= ConstraintSet.insertTypeEquality typeA typeB
