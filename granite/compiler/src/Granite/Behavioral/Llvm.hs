{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens ((^.), (<<%=), at, makeLenses, to, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.RWS (RWST, evalRWST)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.List (genericLength)
import Data.Traversable (for)
import LLVM.AST (Operand)
import LLVM.IRBuilder (IRBuilderT, MonadIRBuilder, MonadModuleBuilder)

import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Short as BS.S
import qualified Data.HashMap.Strict as HashMap
import qualified LLVM.AST.Constant as IR
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Operand as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Abstract (Expression (..), ExpressionPayload (..))
import Granite.Common.Name (Name)
import Granite.Common.Position (Position)

-- TODO: Move this to a module with orphan instances.
instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m)

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

--------------------------------------------------------------------------------
-- Lambdas

-- |
-- Continuation-passing style function for building a lambda. The continuation
-- must build the lambda body.
buildLambda :: forall m
             . ( MonadReader Environment m
               , MonadState State m
               , MonadIRBuilder m
               , MonadModuleBuilder m )
            => Name
            -> RWST Environment () State (IRBuilderT m) Operand
            -> m Operand
buildLambda parameter bodyAction = do
  -- Retrieve used infrastructure.
  rts       <- view envRts
  variables <- view envVariables
  captures  <- view (envVariables . to lambdaCaptures)

  -- Define lambda implementation.
  name <- freshLambdaName
  impl <- IRB.function (IR.Name name) lambdaParams valueType
                       (irbFunctionCallback rts variables captures name)

  -- Construct lambda value.
  buildLambdaConstruction impl (snd <$> captures)
  where
  irbFunctionCallback :: Rts -> HashMap Name Variable -> [(Name, Operand)]
                      -> ShortByteString -> [Operand] -> IRBuilderT m ()
  irbFunctionCallback rts variables captures name [heap, self, argument] =
    let
      globals :: [(Name, Operand)]
      globals = [ (n, o) | (n, Global o) <- HashMap.toList variables ]

      args :: BuildLambdaBodyArgs (IRBuilderT m)
      args = BuildLambdaBodyArgs
        { -- Configuration
          blRts = rts
        , blLambdaNamePrefix = name

          -- Lambda arguments
        , blHeap     = heap
        , blSelf     = self
        , blArgument = argument

          -- Code generation
        , blGlobals   = globals
        , blCaptures  = fst <$> captures
        , blParameter = parameter
        , blAction    = bodyAction
        }
    in
      buildLambdaBody args

data BuildLambdaBodyArgs m =
  BuildLambdaBodyArgs
    { -- Configuration
      blRts :: Rts
    , blLambdaNamePrefix :: ShortByteString

      -- Lambda arguments
    , blHeap     :: Operand
    , blSelf     :: Operand
    , blArgument :: Operand

      -- Code generation
    , blGlobals   :: [(Name, Operand)]
    , blCaptures  :: [Name]
    , blParameter :: Name
    , blAction    :: RWST Environment () State m Operand }

-- |
-- Build the body of a lambda.
buildLambdaBody :: (MonadIRBuilder m, MonadModuleBuilder m)
                => BuildLambdaBodyArgs m -> m ()
buildLambdaBody bl = do
  let globals = [ (n, Global o) | (n, o) <- blGlobals bl ]
  locals <- buildLambdaCapturesToLocals (blRts bl) (blCaptures bl) (blSelf bl)
  let parameters = [(blParameter bl, Local (blArgument bl))]
  let variables = HashMap.fromList (globals <> locals <> parameters)

  let env = Environment
        { _envRts              = blRts bl
        , _envLambdaNamePrefix = blLambdaNamePrefix bl
        , _envHeap             = blHeap bl
        , _envVariables        = variables }

  let build = IRB.ret =<< blAction bl
  ((), ()) <- evalRWST build env (State 0)
  pure ()

-- |
-- Build a construction of a lambda given its implementation and captured
-- values.
buildLambdaConstruction :: ( MonadReader Environment m
                           , MonadIRBuilder m
                           , MonadModuleBuilder m )
                        => Operand -> [Operand] -> m Operand
buildLambdaConstruction impl captures = do
  -- Retrieve used infrastructure.
  graRtsConstructLambda <- view (envRts . rtsConstructLambda)
  heap <- view envHeap

  -- Allocate array of captures.
  let { captureCount :: Integral a => a; captureCount = genericLength captures }
  captures' <- IRB.alloca (IR.ArrayType captureCount valueType) Nothing autoAlign
  captureCount' <- IRB.int64 captureCount

  -- Initialize array of captures.
  for_ (captures `zip` [0 ..]) $ \(capture, i) -> do
    addr <- IRB.gep captures' =<< sequence [IRB.int64 0, IRB.int64 i]
    IRB.store addr autoAlign capture

  -- Generate call.
  captures'' <- IRB.gep captures' =<< sequence [IRB.int64 0, IRB.int64 0]
  IRB.call graRtsConstructLambda ((, []) <$> [heap, impl, captures'', captureCount'])

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
-- Captures of a lambda. The returned operands are those at the lambda
-- construction site, not those inside the lambda body.
lambdaCaptures :: HashMap Name Variable -> [(Name, Operand)]
lambdaCaptures vs = [ (n, o) | (n, Local o) <- HashMap.toList vs ]

-- |
-- Read the captures of a lambda into locals.
buildLambdaCapturesToLocals :: (MonadIRBuilder m, MonadModuleBuilder m)
                            => Rts -> [Name] -> Operand -> m [(Name, Variable)]
buildLambdaCapturesToLocals rts captures self = do
  pointers <- IRB.call (rts ^. rtsValuePointers) [(self, [])]
  for (captures `zip` [0 ..]) $ \(capture, i) -> do
    pointerPointer <- IRB.gep pointers =<< sequence [IRB.int64 i]
    pointer <- IRB.load pointerPointer autoAlign
    pure (capture, Local pointer)

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

--------------------------------------------------------------------------------

-- |
-- Some functions from IRB take alignment. 0 means automatic.
autoAlign :: Integral a => a
autoAlign = 0
