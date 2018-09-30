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

import Control.Lens ((^.), (<<%=), at, makeLenses, to, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.RWS (evalRWST)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.List (genericLength)
import Data.Traversable (for)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Short as BS.S
import qualified Data.HashMap.Strict as HashMap
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
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
    irbFunctionCallback :: ( MonadError Error m, MonadIRBuilder m
                           , MonadModuleBuilder m )
                        => Rts -> HashMap Name Variable -> [(Name, Operand)]
                        -> ShortByteString -> [Operand] -> m ()
    irbFunctionCallback rts variables captures name [heap, self, argument] =
      let
        globals :: [(Name, Operand)]
        globals = [ (n, o) | (n, Global o) <- HashMap.toList variables ]

        args :: BuildLambdaBodyArgs
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
          , blBody      = body
          }
      in
        buildLambdaBody args

  ForeignExpression source type_ ->
    undefined

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

data BuildLambdaBodyArgs =
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
    , blBody      :: Expression 0 }

-- |
-- Build the body of a lambda.
buildLambdaBody :: (MonadError Error m, MonadIRBuilder m, MonadModuleBuilder m)
                => BuildLambdaBodyArgs -> m ()
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

  let build = IRB.ret =<< buildExpression (blBody bl)
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
    pointer <- IRB.gep pointers =<< sequence [IRB.int64 0, IRB.int64 i]
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

--------------------------------------------------------------------------------

-- |
-- Some functions from IRB take alignment. 0 means automatic.
autoAlign :: Integral a => a
autoAlign = 0
