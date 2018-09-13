-- |
-- Like 'Either', but accumulate errors.
module Data.Validation
  ( Validation (..)
  , valid
  , invalid
  ) where

import Data.Semigroup (Semigroup, (<>))

-- |
-- Like 'Either', but accumulate errors.
newtype Validation a b =
  Validation (Either a b)
  deriving newtype (Functor)

instance Semigroup a => Applicative (Validation a) where
  pure = valid
  Validation (Left  a) <*> Validation (Left  b) = Validation (Left (a <> b))
  Validation (Left  a) <*> Validation (Right _) = Validation (Left a)
  Validation (Right _) <*> Validation (Left  b) = Validation (Left b)
  Validation (Right a) <*> Validation (Right b) = Validation (Right (a b))

-- |
-- Like 'pure', but without 'Semigroup' constraint.
valid :: b -> Validation a b
valid = Validation . Right

-- |
-- Construct invalid result.
invalid :: a -> Validation a b
invalid = Validation . Left
