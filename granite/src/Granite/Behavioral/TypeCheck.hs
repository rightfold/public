module Granite.Behavioral.TypeCheck
  ( Error (..)
  , typeCheckExpression
  ) where

import Data.Bifunctor (bimap)
import Data.Functor (void)

import Granite.Behavioral.Abstract (Expression)
import Granite.Behavioral.Infer (assert, infer, runInfer)
import Granite.Behavioral.Type (Type)
import Granite.Organizational.Interface (Interface (..))

import qualified Granite.Behavioral.Constraint as Constraint
import qualified Granite.Behavioral.Infer as Infer

data Error
  = InferError Infer.Error
  | SolveError Constraint.SolveError
  deriving stock (Eq, Show)

-- |
-- Type check an expression, optionally asserting that it has a given type.
typeCheckExpression :: Interface -> Maybe Type -> Expression 0 -> Either Error ()
typeCheckExpression (Interface interface) expected expression = do
  constraints <-
    bimap InferError fst $
      let environment = Infer.Environment (snd <$> interface) in
      flip runInfer environment $
        maybe (void . infer) assert expected expression

  bimap SolveError id $
    Constraint.solve constraints
