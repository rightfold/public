-- |
-- Unification of types, resolving of unknowns.
module Granite.Behavioral.Unify
  ( Error (..)
  , Environment
  , runUnify
  , unify
  , purge
  ) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.HashTable.ST.Basic (HashTable)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.HashTable.ST.Basic as HashTable

import Granite.Behavioral.Type (Type (..), Unknown)

-- |
-- Unification error.
data Error
  = CannotUnify Type Type
  deriving stock (Eq, Show)

-- |
-- Environment for unification.
newtype Environment s =
  Environment
    { envSolutions :: HashTable s Unknown Type }

-- |
-- Run a unification action in a new environment.
runUnify :: (forall s. ReaderT (Environment s) (ExceptT Error (ST s)) a)
         -> Either Error a
runUnify action = runST $ do
  env <- Environment <$> HashTable.new
  runExceptT (runReaderT action env)

-- |
-- Ensure that two types are the same, solving unknown if needed.
unify :: ( MonadError Error m
         , MonadReader (Environment (PrimState m)) m
         , PrimMonad m )
      => Type -> Type -> m ()
unify t1 t2 = do
  t1' <- purge t1
  t2' <- purge t2
  unify' t1' t2'

-- |
-- Like 'unify', but do not first purge the inputs.
unify' :: ( MonadError Error m
          , MonadReader (Environment (PrimState m)) m
          , PrimMonad m )
       => Type -> Type -> m ()
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
purge :: (MonadReader (Environment (PrimState m)) m, PrimMonad m)
      => Type -> m Type
purge t@(UnknownType u) = do
  solutions <- Reader.asks envSolutions
  maybe (pure t) purge =<< stToPrim (HashTable.lookup solutions u)
purge t = pure t

recordSolution :: (MonadReader (Environment (PrimState m)) m, PrimMonad m)
               => Unknown -> Type -> m ()
recordSolution u t = do
  solutions <- Reader.asks envSolutions
  stToPrim $ HashTable.insert solutions u t
