module Granite.Behavioral.Llvm.Foreign
  ( buildForeign
  , buildMarshal
  , buildUnmarshal
  ) where

import Control.Lens ((?~), at, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.ByteString.Short as BS.S
import qualified Data.Text as Text
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Llvm.Infrastructure

import Granite.Behavioral.Llvm.Lambda (buildLambda)
import Granite.Behavioral.Type (pattern F64Type, pattern PointerType, pattern U8Type, pattern U64Type, Type (..), dissectFunctionType)
import Granite.Common.Name (Name (..))

buildForeign :: ( MonadError Error m
                , MonadReader Environment m
                , MonadState State m
                , MonadIRBuilder m
                , MonadModuleBuilder m )
             => Text -> Type -> m Operand
buildForeign source type_ =
  go parameterNames
  where
  go :: ( MonadError Error m
        , MonadReader Environment m
        , MonadState State m
        , MonadIRBuilder m
        , MonadModuleBuilder m )
     => [Name] -> m Operand

  go (parameter : parameters) =
    buildLambda $ \argument ->
      Reader.local (envVariables . at parameter ?~ Local argument) $
        go parameters

  go [] = do
    -- FIXME: This is incomprehensible garbage.
    arguments <- for parameterNames $ \parameterName -> do
                   variable <- view (envVariables . at parameterName)
                   case variable of
                     Just (Local operand) -> pure operand
                     a -> error ("buildForeign: " <> show a)
    buildActualCall source returnType (arguments `zip` parameterTypes)

  parameterNames :: [Name]
  parameterTypes :: [Type]
  returnType     :: Type
  parameterNames = take (length parameterTypes) $
                     PlainName . Text.pack . show @Int <$> [0 ..]
  (parameterTypes, returnType) = dissectFunctionType type_

buildActualCall :: ( MonadError Error m
                   , MonadReader Environment m
                   , MonadIRBuilder m
                   , MonadModuleBuilder m )
                => Text -> Type -> [(Operand, Type)] -> m Operand
buildActualCall name returnType arguments = do
  parameterTypes' <- traverse (llvmType . snd) arguments
  returnType'     <- llvmType returnType

  arguments' <- traverse (uncurry (flip buildMarshal)) arguments

  let name' = IR.Name (BS.S.toShort (encodeUtf8 name))
  callee <- IRB.extern name' parameterTypes' returnType'
  result <- IRB.call callee ((, []) <$> arguments')

  buildUnmarshal returnType result

buildMarshal :: ( MonadError Error m
                , MonadReader Environment m
                , MonadIRBuilder m )
             => Type -> Operand -> m Operand

buildMarshal U64Type value = do
  graReadU64 <- view (envRts . rtsReadU64)
  IRB.call graReadU64 [(value, [])]

buildMarshal (PointerType _) value = do
  graReadPointer <- view (envRts . rtsReadPointer)
  -- XXX: Do we have to coerce the pointer from u8*?
  IRB.call graReadPointer [(value, [])]

buildMarshal t _ =
  throwError (CannotMarshalType t)

buildUnmarshal :: ( MonadError Error m
                  , MonadReader Environment m
                  , MonadIRBuilder m )
               => Type -> Operand -> m Operand

buildUnmarshal U8Type value = do
  heap <- view envHeap
  graConstructU8 <- view (envRts . rtsConstructU8)
  IRB.call graConstructU8 [(heap, []), (value, [])]

buildUnmarshal U64Type value = do
  heap <- view envHeap
  graConstructU64 <- view (envRts . rtsConstructU64)
  IRB.call graConstructU64 [(heap, []), (value, [])]

buildUnmarshal (PointerType _) value = do
  heap <- view envHeap
  graConstructPointer <- view (envRts . rtsConstructPointer)
  -- XXX: Do we have to coerce the pointer to u8*?
  IRB.call graConstructPointer [(heap, []), (value, [])]

buildUnmarshal t _ =
  throwError (CannotMarshalType t)

llvmType :: MonadError Error m => Type -> m IR.Type
llvmType t = llvmType' t & maybe (throwError (CannotMarshalType t)) pure

llvmType' :: Type -> Maybe IR.Type
llvmType' U8Type          = Just IR.i8
llvmType' U64Type         = Just IR.i64
llvmType' F64Type         = Just (IR.FloatingPointType IR.DoubleFP)
llvmType' (PointerType t) = IR.ptr <$> llvmType' t
llvmType' _ = Nothing
