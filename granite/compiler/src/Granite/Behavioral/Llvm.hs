{-# LANGUAGE TemplateHaskell #-}

-- |
-- Build LLVM IR from expressions.
module Granite.Behavioral.Llvm
  ( -- * Infrastructure
    Error (..)
  , Environment (..)

    -- * Building
  , buildExpression
  ) where

import Control.Lens (at, makeLenses, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Data.HashMap.Strict (HashMap)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Common.Name (Name)
import Granite.Common.Position (Position)

--------------------------------------------------------------------------------
-- Infrastructure

-- |
-- Error that occurs during building.
data Error
  = UnknownName Position Name
  deriving stock (Eq, Show)

data Variable
  = Local Operand
  | Global Operand

data Environment =
  Environment
    { _envVariables :: HashMap Name Variable }
$(makeLenses ''Environment)

--------------------------------------------------------------------------------
-- Building

-- |
-- Build an expression, returning its result.
buildExpression :: ( MonadError Error m
                   , MonadReader Environment m
                   , MonadIRBuilder m
                   , MonadModuleBuilder m )
                => Expression 0 -> m Operand
buildExpression (Expression position payload) = case payload of

  VariableExpression name ->
    view (envVariables . at name) >>= \case
      Nothing -> throwError $ UnknownName position name
      Just (Local operand)  -> pure operand
      Just (Global operand) -> buildGlobalGet operand

  ApplicationExpression function argument -> do
    function' <- buildExpression function
    argument' <- buildExpression argument
    buildCall function' argument'

  LambdaExpression parameter body ->
    -- TODO: Do something with MonadModuleBuilder.
    undefined

  ForeignExpression source type_ ->
    undefined

-- |
-- Build an expression that gets a global. Globals are defined as LLVM
-- functions that take only a heap as argument.
buildGlobalGet :: Operand -> m Operand
buildGlobalGet = undefined

-- |
-- Build a call. Callees take three arguments: a heap, the closure that is
-- called, and the argument that the closure is called with.
buildCall :: Operand -> Operand -> m Operand
buildCall = undefined
