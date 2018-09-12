-- |
-- Data types for parts of articles.
module Krokodil.Article
  ( ArticleTitle (..)
  , ArticleBody (..)
  , ArticleBodyFont (..)
  ) where

import qualified Data.ByteString as Bs

-- |
-- A title of an article.
newtype ArticleTitle =
  ArticleTitle Bs.ByteString

-- |
-- A body of an article.
data ArticleBody =
  ArticleBody
    { articleBodyText :: Bs.ByteString
    , articleBodyFont :: ArticleBodyFont }

-- |
-- Bodies can be typeset in one of a set of fonts.
data ArticleBodyFont
  = ProportionalFont -- ^ The body is to be typeset in a proportional font.
  | MonospaceFont    -- ^ The body is to be typeset in a monospace font.
