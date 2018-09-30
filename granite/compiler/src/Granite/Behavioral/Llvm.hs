{-# LANGUAGE TemplateHaskell #-}

-- |
-- Build LLVM IR from expressions.
module Granite.Behavioral.Llvm
  ( -- * Infrastructure
    Error (..)
  , Rts (..)
  , Environment (..)
  , State

    -- * Building
  , buildExpression
  ) where

import Control.Lens ((<<%=), at, makeLenses, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.ByteString.Short (ShortByteString)
import Data.HashMap.Strict (HashMap)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Short as BS.S
import qualified LLVM.AST as IR
import qualified LLVM.AST.AddrSpace as IR
import qualified LLVM.IRBuilder as IRB

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

data Rts =
  Rts
    { _rtsCallLambda :: Operand }
$(makeLenses ''Rts)

data Environment =
  Environment
    { -- |
      -- The variables that are in scope, with their corresponding LLVM values.
      _envVariables :: HashMap Name Variable

      -- |
      -- The heap to use for memory management.
    , _envHeap :: Operand

    , _envRts :: Rts

      -- |
      -- The prefix to use for names of lambda implementations.
    , _envLambdaNamePrefix :: ShortByteString }
$(makeLenses ''Environment)

data State =
  State
    { _stateNextLambdaId :: Word }
$(makeLenses ''State)

freshLambdaName :: (MonadReader Environment m, MonadState State m)
                => m ShortByteString
freshLambdaName = do
  prefix <- view envLambdaNamePrefix
  uniqueId <- stateNextLambdaId <<%= succ
  pure (prefix <> "$" <> sbshow uniqueId)
  where sbshow = BS.S.toShort . BS.C8.pack . show

--------------------------------------------------------------------------------
-- Building

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
    name <- freshLambdaName
    impl <- IRB.function (IR.Name name) lambdaParams valueType $
              \[heap, self, argument] ->
                undefined -- TODO: Build body.
    undefined -- TODO: Build closure.

  ForeignExpression source type_ ->
    undefined

-- |
-- Build an expression that gets a global.
buildGlobalGet :: (MonadReader Environment m, MonadIRBuilder m)
               => Operand -> m Operand
buildGlobalGet global = do
  heap <- view envHeap
  IRB.call global [(heap, [])]

-- |
-- Build a call to a lambda.
buildLambdaCall :: (MonadReader Environment m, MonadIRBuilder m)
                => Operand -> Operand -> m Operand
buildLambdaCall function argument = do
  graRtsCallLambda <- view (envRts . rtsCallLambda)
  heap <- view envHeap
  IRB.call graRtsCallLambda ((, []) <$> [heap, function, argument])

-- |
-- Parameters of lambdas.
lambdaParams :: [(IR.Type, IRB.ParameterName)]
lambdaParams = (, IRB.NoParameterName) <$> [heapType, valueType, valueType]

-- |
-- Type of heaps.
heapType :: IR.Type
heapType = IR.PointerType (IR.IntegerType 8) (IR.AddrSpace 0)

-- |
-- Type of values.
valueType :: IR.Type
valueType = IR.PointerType (IR.IntegerType 8) (IR.AddrSpace 0)
