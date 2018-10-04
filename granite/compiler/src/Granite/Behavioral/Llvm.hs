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

import Control.Lens ((?~), at, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified Control.Monad.Reader.Class as Reader
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Llvm.Infrastructure

import Granite.Behavioral.Abstract (Builtin (..), Expression (..), ExpressionPayload (..))
import Granite.Behavioral.Llvm.Foreign (buildForeign, buildUnmarshal)
import Granite.Behavioral.Llvm.Lambda (buildLambda, buildLambdaCall)
import Granite.Behavioral.Type (pattern PointerType, pattern U8Type, pattern U64Type, typeFromExpression)

--------------------------------------------------------------------------------
-- Building

buildRts :: MonadModuleBuilder m => m Rts
buildRts = do
  valuePointers <-
    IRB.extern
      (IR.Name "_ZN3gra5value8pointersEv")
      [valueType]
      (IR.ptr valueType)

  valueAuxiliarySize <-
    IRB.extern
      (IR.Name "_ZNK3gra5value13auxiliarySizeEv")
      [valueType]
      IR.i64

  valueAuxiliary <-
    IRB.extern
      (IR.Name "_ZN3gra5value9auxiliaryEv")
      [valueType]
      (IR.ptr IR.i8)

  constructU8 <-
    IRB.extern
      (IR.Name "_ZN3gra11constructU8ERNS_4heapEh")
      [heapType, IR.i8]
      valueType

  constructU64 <-
    IRB.extern
      (IR.Name "_ZN3gra12constructU64ERNS_4heapEm")
      [heapType, IR.i64]
      valueType

  readU64 <-
    IRB.extern
      (IR.Name "_ZN3gra7readU64EPNS_5valueE")
      [valueType]
      IR.i64

  constructPointer <-
    IRB.extern
      (IR.Name "_ZN3gra16constructPointerERNS_4heapEPv")
      [heapType, IR.ptr IR.i8]
      valueType

  readPointer <-
    IRB.extern
      (IR.Name "_ZN3gra11readPointerEPNS_5valueE")
      [valueType]
      (IR.ptr IR.i8)

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

  pure $ Rts { _rtsValuePointers      = valuePointers
             , _rtsValueAuxiliarySize = valueAuxiliarySize
             , _rtsValueAuxiliary     = valueAuxiliary

             , _rtsConstructU8        = constructU8
             , _rtsConstructU64       = constructU64
             , _rtsReadU64            = readU64

             , _rtsConstructPointer   = constructPointer
             , _rtsReadPointer        = readPointer

             , _rtsConstructLambda    = constructLambda
             , _rtsCallLambda         = callLambda }


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
    buildLambda $ \argument ->
      Reader.local (envVariables . at parameter ?~ Local argument) $
        buildExpression body

  BuiltinExpression BuiltinAuxiliary -> do
    graValueAuxiliary <- view (envRts . rtsValueAuxiliary)
    buildLambda $ \argument -> do
      result <- IRB.call graValueAuxiliary [(argument, [])]
      buildUnmarshal (PointerType U8Type) result

  BuiltinExpression BuiltinAuxiliarySize -> do
    graValueAuxiliarySize <- view (envRts . rtsValueAuxiliarySize)
    buildLambda $ \argument -> do
      result <- IRB.call graValueAuxiliarySize [(argument, [])]
      buildUnmarshal U64Type result

  BuiltinExpression BuiltinCoerce ->
    buildLambda pure

  BuiltinExpression BuiltinEffJoin ->
    _

  BuiltinExpression BuiltinEffMap ->
    _

  BuiltinExpression BuiltinEffPure ->
    _

  ForeignExpression source type_ ->
    buildForeign source (typeFromExpression type_)

--------------------------------------------------------------------------------
-- Globals

-- |
-- Build an expression that gets a global.
buildGlobalGet :: (MonadReader Environment m, MonadIRBuilder m)
               => Operand -> m Operand
buildGlobalGet global = do
  heap <- view envHeap
  IRB.call global [(heap, [])]
