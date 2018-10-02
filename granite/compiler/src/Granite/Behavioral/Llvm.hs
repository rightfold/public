-- |
-- Build LLVM IR from expressions.
module Granite.Behavioral.Llvm
  ( -- * Infrastructure
    Error (..)
  , Variable (..)
  , Rts (..)
  , Environment (..)
  , State (..)

    -- * Building
  , buildRts
  , buildExpression

    -- * Types
  , heapType
  , valueType
  ) where

import Control.Lens (at, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified LLVM.AST.Constant as IR
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Operand as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Llvm.Infrastructure

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Behavioral.Llvm.Lambda (buildLambda, buildLambdaCall)

--------------------------------------------------------------------------------
-- Building

buildRts :: MonadModuleBuilder m => m Rts
buildRts = do
  constructLambda <-
    IRB.extern
      (IR.Name "_ZN3gra15constructLambdaERNS_4heapEPFPNS_5valueES1_S3_S3_EPS3_m")
      [heapType, lambdaCodeType, IR.ptr valueType, IR.i64]
      valueType

  callLambda <-
    IRB.extern
      (IR.Name "_ZN3gra10callLambdaERNS_4heapEPNS_5valueES3_")
      [heapType, valueType, valueType]
      valueType

  valuePointers <-
    IRB.extern
      (IR.Name "_ZN3gra5value8pointersEv")
      [valueType]
      (IR.ptr valueType)

  pure $ Rts { _rtsConstructLambda = constructLambda
             , _rtsCallLambda      = callLambda
             , _rtsValuePointers   = valuePointers }


-- |
-- Build an expression, returning its result.
buildExpression :: ( MonadError Error m
                   , MonadReader Environment m
                   , MonadState State m
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
    buildLambdaCall function' argument'

  LambdaExpression parameter body -> do
    buildLambda parameter (buildExpression body)

  ForeignExpression source type_ ->
    -- TODO: Implement foreign expressions.
    pure (IR.ConstantOperand (IR.Null valueType))

--------------------------------------------------------------------------------
-- Globals

-- |
-- Build an expression that gets a global.
buildGlobalGet :: (MonadReader Environment m, MonadIRBuilder m)
               => Operand -> m Operand
buildGlobalGet global = do
  heap <- view envHeap
  IRB.call global [(heap, [])]
