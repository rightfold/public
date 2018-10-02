{-# LANGUAGE TemplateHaskell #-}

module Granite.Behavioral.Llvm.Infrastructure where

import Control.Lens ((<<%=), makeLenses, view)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.ByteString.Short (ShortByteString)
import Data.HashMap.Strict (HashMap)
import LLVM.AST (Operand)

import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Short as BS.S
import qualified LLVM.AST.Type as IR

import Granite.Common.Name (Name)
import Granite.Common.Position (Position)

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
    { _rtsConstructLambda :: Operand
    , _rtsCallLambda      :: Operand
    , _rtsValuePointers   :: Operand }
$(makeLenses ''Rts)

data Environment =
  Environment
    { _envRts              :: Rts
    , _envLambdaNamePrefix :: ShortByteString
    , _envHeap             :: Operand
    , _envVariables        :: HashMap Name Variable }
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
  pure (prefix <> "$lambda" <> sbshow uniqueId)
  where sbshow = BS.S.toShort . BS.C8.pack . show

-- |
-- Some functions from IRB take alignment. 0 means automatic.
autoAlign :: Integral a => a
autoAlign = 0

--------------------------------------------------------------------------------
-- Types

-- |
-- Type of heaps.
heapType :: IR.Type
heapType = IR.ptr (IR.IntegerType 8)

-- |
-- Type of values.
valueType :: IR.Type
valueType = IR.ptr (IR.IntegerType 8)

-- |
-- Type of pointers to lambda code.
lambdaCodeType :: IR.Type
lambdaCodeType = IR.ptr (IR.FunctionType valueType params False)
                   where params = [heapType, valueType, valueType]
