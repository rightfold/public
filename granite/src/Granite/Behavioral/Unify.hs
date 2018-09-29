{-# LANGUAGE TemplateHaskell #-}

-- |
-- Unification of types, resolving of unknowns.
module Granite.Behavioral.Unify
  ( Error (..)
  , State
  , runUnify
  , unify
  , purge
  ) where

import Control.Lens ((?=), at, makeLenses, use)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap

import Granite.Behavioral.Type (Type (..), Unknown)

-- |
-- Unification error.
data Error
  = CannotUnify Type Type
  deriving stock (Eq, Show)

-- |
-- State for unification.
newtype State = State { _stateSolutions :: HashMap Unknown Type }
$(makeLenses ''State)

-- |
-- Run a unification action in a new environment.
runUnify :: StateT State (Either Error) a -> Either Error a
runUnify action = evalStateT action (State HashMap.empty)

-- |
-- Ensure that two types are the same, solving unknown if needed.
unify :: (MonadError Error m, MonadState State m) => Type -> Type -> m ()
unify t1 t2 = do
  t1' <- purge t1
  t2' <- purge t2
  unify' t1' t2'

-- |
-- Like 'unify', but do not first purge the inputs.
unify' :: (MonadError Error m, MonadState State m) => Type -> Type -> m ()

unify' (UnknownType u1) (UnknownType u2) | u1 == u2 = pure ()
unify' (UnknownType u) t = recordSolution u t
unify' t (UnknownType u) = recordSolution u t

unify' (SkolemType s1) (SkolemType s2) | s1 == s2 = pure ()
unify' t1@(SkolemType _) t2 = throwError $ CannotUnify t1 t2
unify' t1 t2@(SkolemType _) = throwError $ CannotUnify t1 t2

unify' (VariableType s1) (VariableType s2) | s1 == s2 = pure ()
unify' t1@(VariableType _) t2 = throwError $ CannotUnify t1 t2
unify' t1 t2@(VariableType _) = throwError $ CannotUnify t1 t2

unify' (ApplicationType f1 a1) (ApplicationType f2 a2) = do
  unify f1 f2
  unify a1 a2

-- |
-- Replace solved unknowns by their solutions.
purge :: MonadState State m => Type -> m Type
purge t@(UnknownType u) = maybe (pure t) purge =<< use (stateSolutions . at u)
purge t = pure t

recordSolution :: MonadState State m => Unknown -> Type -> m ()
recordSolution u t = stateSolutions . at u ?= t
