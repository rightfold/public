module Granite.Behavioral.TypeCheck
  ( Error (..)
  , typeCheckExpression
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Bifunctor (bimap)

import qualified Data.Primitive.MutVar as MutVar

import Granite.Behavioral.Abstract (Expression)
import Granite.Behavioral.Infer (infer)
import Granite.Organizational.Interface (Interface (..))

import qualified Granite.Behavioral.Constraint as ConstraintSet
import qualified Granite.Behavioral.Infer as Infer

data Error
  = InferError Infer.Error
  deriving stock (Eq, Show)

typeCheckExpression :: Interface -> Expression 0 -> Either Error ()
typeCheckExpression (Interface interface) expression =
  bimap InferError (const ()) $
    runST $ do
      env <- environment
      flip runReaderT env $
        runExceptT $
          infer expression
  where
  environment :: PrimMonad m => m (Infer.Environment (PrimState m))
  environment = do
    let variables = fmap snd interface
    constraints <- ConstraintSet.newConstraintSet
    nextFresh <- MutVar.newMutVar 0
    pure $ Infer.Environment variables constraints nextFresh
