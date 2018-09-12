-- |
-- HTML-related utilities.
module Krokodil.Html
  ( -- * Escaping
    escapeHtml
  , escapeHtmlChar
  ) where

import Data.Word (Word8)

import qualified Data.ByteString.Builder as Bsb
import qualified Data.ByteString.Lazy as Lbs

-- |
-- Escape a byte string. This escapes the characters @&@ and @<@.
escapeHtml :: Lbs.ByteString -> Bsb.Builder
escapeHtml = foldMap escapeHtmlChar . Lbs.unpack

-- |
-- Escape a character. This escapes the characters @&@ and @<@.
escapeHtmlChar :: Word8 -> Bsb.Builder
escapeHtmlChar 0x26 = "&amp;"
escapeHtmlChar 0x3c = "&lt;"
escapeHtmlChar c    = Bsb.word8 c
