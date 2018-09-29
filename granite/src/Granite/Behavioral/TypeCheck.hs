module Granite.Behavioral.TypeCheck
  ( Error (..)
  , typeCheckExpression
  ) where

import Data.Bifunctor (bimap)

import Granite.Behavioral.Abstract (Expression)
import Granite.Behavioral.Infer (infer, runInfer)
import Granite.Organizational.Interface (Interface (..))

import qualified Granite.Behavioral.Infer as Infer

data Error
  = InferError Infer.Error
  deriving stock (Eq, Show)

typeCheckExpression :: Interface -> Expression 0 -> Either Error ()
typeCheckExpression (Interface interface) expression =
  bimap InferError (const ()) $
    let environment = Infer.Environment (snd <$> interface) in
    runInfer (infer expression) environment
