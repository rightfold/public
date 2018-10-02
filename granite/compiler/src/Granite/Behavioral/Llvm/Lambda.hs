module Granite.Behavioral.Llvm.Lambda
  ( buildLambda
  , buildLambdaCall
  ) where

import Control.Lens ((^.), to, view)
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

import qualified Data.HashMap.Strict as HashMap
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Llvm.Infrastructure

import Granite.Common.Name (Name)

-- TODO: Move this to a module with orphan instances.
instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m)

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
