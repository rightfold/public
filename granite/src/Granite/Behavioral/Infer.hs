-- |
-- Infer types of expressions.
module Granite.Behavioral.Infer
  ( -- * Infrastructure
    Error (..)
  , Environment (..)
  , freshUnknown
  , freshSkolem

    -- * Inference
  , infer
  ) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Reader.Class (MonadReader)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.MutVar (MutVar)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Primitive.MutVar as MutVar

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
-- Environment in which to infer types. Also contains the constraint set.
data Environment s =
  Environment
    { envVariables   :: HashMap Name Type
    , envConstraints :: ConstraintSet s
    , envNextFresh   :: MutVar s Word }

-- |
-- Generate a fresh unknown.
freshUnknown :: (MonadReader (Environment (PrimState m)) m, PrimMonad m)
             => m Unknown
freshUnknown = do
  var <- Reader.asks envNextFresh
  MutVar.modifyMutVar' var succ
  Unknown <$> MutVar.readMutVar var

-- |
-- Generate a fresh Skolem.
freshSkolem :: (MonadReader (Environment (PrimState m)) m, PrimMonad m)
             => m Skolem
freshSkolem = do
  var <- Reader.asks envNextFresh
  MutVar.modifyMutVar' var succ
  Skolem <$> MutVar.readMutVar var

-- |
-- Infer the type of an expression, collecting constraints.
infer :: ( MonadError Error m
         , MonadReader (Environment (PrimState m)) m
         , PrimMonad m )
      => Expression 0 -> m Type
infer (Expression position payload) = case payload of

  VariableExpression name -> do
    type_ <- Reader.asks (HashMap.lookup name . envVariables)
    maybe (throwError (UnknownName position name)) pure type_

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

    let insertParameter = HashMap.insert parameter parameterType
    let insertParameter' e = e { envVariables = insertParameter (envVariables e) }
    returnType <- Reader.local insertParameter' (infer body)

    pure $ makeFunctionType parameterType returnType

makeFunctionType :: Type -> Type -> Type
makeFunctionType parameterType returnType =
  ApplicationType (ApplicationType (VariableType arrow) parameterType) returnType
  where arrow = InfixName InfixHyphenGreater

insertTypeEquality :: (MonadReader (Environment (PrimState m)) m, PrimMonad m)
                   => Type -> Type -> m ()
insertTypeEquality typeA typeB = do
  constraints <- Reader.asks envConstraints
  ConstraintSet.insertTypeEquality constraints typeA typeB
