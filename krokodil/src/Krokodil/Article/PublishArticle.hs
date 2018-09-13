-- |
-- Implementation for the /Publish article/ use case.
module Krokodil.Article.PublishArticle
  ( Validation
  , ValidationError (..)
  , validateArticle
  , validateArticleTitle
  , validateArticleBody
  , validateArticleBodyText
  , validateArticleBodyFont
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Validation (invalid, valid)

import qualified Data.ByteString as Bs
import qualified Data.Validation as V

import Krokodil.Article (ArticleBody (..), ArticleBodyFont (..), ArticleTitle (..))

type Validation =
  V.Validation (NonEmpty ValidationError)

data ValidationError
  = EmptyTitle
  | EmptyBodyText
  | UnknownBodyFont
  deriving stock (Bounded, Enum, Eq, Ord, Show)

validateArticle :: Bs.ByteString -> Bs.ByteString -> Bs.ByteString
                -> Validation (ArticleTitle, ArticleBody)
validateArticle title bodyText bodyFont =
  (,) <$> validateArticleTitle title
      <*> validateArticleBody bodyText bodyFont

validateArticleTitle :: Bs.ByteString -> Validation ArticleTitle
validateArticleTitle "" = invalid (EmptyTitle :| [])
validateArticleTitle t  = valid (ArticleTitle t)

validateArticleBody :: Bs.ByteString -> Bs.ByteString -> Validation ArticleBody
validateArticleBody text font =
  ArticleBody <$> validateArticleBodyText text
              <*> validateArticleBodyFont font

validateArticleBodyText :: Bs.ByteString -> Validation Bs.ByteString
validateArticleBodyText "" = invalid (EmptyBodyText :| [])
validateArticleBodyText t  = valid t

validateArticleBodyFont :: Bs.ByteString -> Validation ArticleBodyFont
validateArticleBodyFont "proportional" = valid ProportionalFont
validateArticleBodyFont "monospace"    = valid MonospaceFont
validateArticleBodyFont _              = invalid (UnknownBodyFont :| [])
