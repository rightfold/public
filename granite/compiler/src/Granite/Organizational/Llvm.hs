-- |
-- Build LLVM IR from definitions.
module Granite.Organizational.Llvm
  ( -- * Infrastructure
    Error (..)
  , Rts (..)

    -- * Building
  , buildInterface
  , buildImplementation
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.RWS (evalRWST)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (fold, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST (Operand)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)

import qualified Data.ByteString.Short as BS.S
import qualified Data.HashMap.Strict as HashMap
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
import qualified LLVM.IRBuilder as IRB

import Granite.Behavioral.Llvm (Error (..), Rts (..), buildExpression, heapType, valueType)
import Granite.Common.Name (Infix (..), Name (..))
import Granite.Organizational.Abstract (Definition (..), DefinitionPayload (..))

import qualified Granite.Behavioral.Llvm as Behavioral

--------------------------------------------------------------------------------
-- Building

buildInterface :: (Traversable f, MonadModuleBuilder m)
               => f Definition -> m (HashMap Name Operand)
buildInterface = fmap (HashMap.fromList . fold) . traverse buildInterface'

buildInterface' :: (MonadModuleBuilder m) => Definition -> m [(Name, Operand)]
buildInterface' (Definition _ payload) = case payload of

  ValueDefinition name (Just _) _ -> do
    let name' = mangleGraniteName name
    global <- IRB.extern (IR.Name name') (fst <$> globalParams) valueType
    pure [(name, global)]

  ValueDefinition _ Nothing _ ->
    pure []

buildImplementation :: (Foldable f, MonadError Error m, MonadModuleBuilder m)
                    => Rts -> HashMap Name Operand -> f Definition -> m ()
buildImplementation rts globals = traverse_ (buildImplementation' rts globals)

buildImplementation' :: (MonadError Error m, MonadModuleBuilder m)
                     => Rts -> HashMap Name Operand -> Definition -> m ()
buildImplementation' rts globals (Definition _ payload) = case payload of

  ValueDefinition name _ (Just body) -> do
    let name' = mangleGraniteName name
    _ <- IRB.function (IR.Name name') globalParams valueType
                      (irbFunctionCallback name')
    pure ()
    where
    irbFunctionCallback :: ( MonadError Error m, MonadIRBuilder m
                           , MonadModuleBuilder m )
                        => ShortByteString -> [Operand] -> m ()
    irbFunctionCallback name [heap] =
      let
        env :: Behavioral.Environment
        env = Behavioral.Environment
          { Behavioral._envRts              = rts
          , Behavioral._envLambdaNamePrefix = name
          , Behavioral._envHeap             = heap
          , Behavioral._envVariables        = Behavioral.Global <$> globals }

        state :: Behavioral.State
        state = Behavioral.State 0
      in
        do { ((), ()) <- evalRWST (IRB.ret =<< buildExpression body) env state
           ; pure () }

  ValueDefinition _ _ Nothing ->
    pure ()

globalParams :: [(IR.Type, IRB.ParameterName)]
globalParams = [(heapType, IRB.NoParameterName)]

--------------------------------------------------------------------------------
-- Name mangling

mangleGraniteName :: Name -> ShortByteString

mangleGraniteName (PlainName name) = "gra$" <> BS.S.toShort (encodeUtf8 name)

mangleGraniteName (InfixName InfixHyphenGreater) = "gra$infix->"

mangleGraniteName (InfixName InfixAsterisk)      = "gra$infix*"
mangleGraniteName (InfixName InfixHyphen)        = "gra$infix-"
mangleGraniteName (InfixName InfixPlus)          = "gra$infix+"
mangleGraniteName (InfixName InfixSolidus)       = "gra$infix/"
