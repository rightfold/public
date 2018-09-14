-- |
-- DSL for publishing articles.
module Krokodil.Article.PublishArticle.DSL
  ( PublishArticle (..)
  , publishArticle
  ) where

import Control.Monad.Free (MonadFree, liftF)

import Krokodil.Article (ArticleBody, ArticleTitle)

data PublishArticle a
  = PublishArticle a ArticleTitle ArticleBody
  deriving stock (Functor)

publishArticle :: MonadFree PublishArticle m => ArticleTitle -> ArticleBody -> m ()
publishArticle title body = liftF (PublishArticle () title body)
