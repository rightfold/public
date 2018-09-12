-- |
-- Implementation for the /Display article/ use case.
module Krokodil.Article.DisplayArticle
  ( -- * Rendering articles
    articleElement
  , articleTitleElement
  , articleBodyElement
  , articleBodyFontClass
  ) where

import Data.Foldable (fold)
import Data.Semigroup ((<>))

import qualified Data.ByteString.Builder as Bb
import qualified Data.ByteString.Lazy as Lbs

import Krokodil.Article (ArticleBody (..), ArticleBodyFont (..), ArticleTitle (..))
import Krokodil.Html (escapeHtml)

-- |
-- The element for an article.
articleElement :: ArticleTitle -> ArticleBody -> Bb.Builder
articleElement title body = fold
  [ "<article class=\"krokodil--article\">"
  ,   articleTitleElement title
  ,   articleBodyElement body
  , "</article>" ]

-- |
-- The element for an article title.
articleTitleElement :: ArticleTitle -> Bb.Builder
articleTitleElement (ArticleTitle text) =
  "<h1>" <> escapeHtml (Lbs.fromStrict text) <> "</h1>"

-- |
-- The element for an article body.
articleBodyElement :: ArticleBody -> Bb.Builder
articleBodyElement (ArticleBody text font) = fold
  [ "<p class=\"" <> articleBodyFontClass font <> "\">"
  , escapeHtml (Lbs.fromStrict text)
  , "</p>" ]

-- |
-- The class for an article body font.
articleBodyFontClass :: ArticleBodyFont -> Bb.Builder
articleBodyFontClass ProportionalFont = "-proportional"
articleBodyFontClass MonospaceFont    = "-monospace"
