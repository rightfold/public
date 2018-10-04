-- |
-- Lexicographical analysis is provided as parsers that parse specific expected
-- tokens, not as a stream of tokens. Tokens already skip whitespace.
--
-- I am aware that having a separate lexer, communicating through a list of
-- tokens, is superior. The problem is that the parser combinator libraries
-- make it a pain to use lists of tokens as inputs to the parsers.
module Granite.Common.Lex
  ( -- * Identifiers
    identifier

    -- * Keywords
  , Keyword (..)
  , keywordSpelling
  , keyword

    -- * Punctuation
  , Punctuation (..)
  , punctuationSpelling
  , punctuation

    -- * Literals
  , textLiteral

    -- * Compound
  , name
  ) where

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Functor (void)
import Data.Text (Text)
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as Parser
import qualified Data.Text as Text

import Granite.Common.Name (Infix (..), Name (..), Prefix (..))
import Granite.Common.Position (Position (..))

--------------------------------------------------------------------------------
-- Identifiers

-- |
-- Lex an identifier.
identifier :: Parser (Position, Text)
identifier = (<?> "identifier") . Parser.try . lexeme $ do
  ident <- (Text.pack .) . (:) <$> identifierHead <*> Parser.many identifierTail
  when (ident `elem` fmap keywordSpelling [minBound .. maxBound]) $
    Parser.unexpected $ "keyword " ++ show ident
  pure ident

identifierHead :: Parser Char
identifierHead = Parser.oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_")

identifierTail :: Parser Char
identifierTail = identifierHead <|> Parser.oneOf (['0' .. '9'] ++ "'!?")

--------------------------------------------------------------------------------
-- Keywords

-- |
-- Keywords.
data Keyword :: * where
  K___builtinAuxiliary :: Keyword
  K___builtinAuxiliarySize :: Keyword
  K___builtinCoerce :: Keyword
  K___builtinEffJoin :: Keyword
  K___builtinEffMap :: Keyword
  K___builtinEffPure :: Keyword
  K_forall :: Keyword
  K_foreign :: Keyword
  K_infix :: Keyword
  K_is :: Keyword
  K_lambda :: Keyword
  K_of :: Keyword
  K_prefix :: Keyword
  K_value :: Keyword
  deriving stock (Eq, Ord, Bounded, Enum, Show)

-- |
-- Given a keyword, its spelling.
keywordSpelling :: Keyword -> Text
keywordSpelling = Text.pack . drop 2 . show

-- |
-- Lex a keyword.
keyword :: Keyword -> Parser Position
keyword kw = fmap fst . lexeme . Parser.try $
  Parser.string (Text.unpack (keywordSpelling kw))
  <* Parser.notFollowedBy identifierTail

--------------------------------------------------------------------------------
-- Punctuation

-- |
-- Punctuation.
data Punctuation :: * where
  P_asterisk :: Punctuation
  P_hyphen :: Punctuation
  P_hyphen_greater :: Punctuation
  P_paren_left :: Punctuation
  P_paren_right :: Punctuation
  P_period :: Punctuation
  P_plus :: Punctuation
  P_semicolon :: Punctuation
  P_solidus :: Punctuation
  deriving stock (Eq, Ord, Bounded, Enum, Show)

-- |
-- Given punctuation, its spelling.
punctuationSpelling :: Punctuation -> Text
punctuationSpelling P_asterisk       = "*"
punctuationSpelling P_hyphen         = "-"
punctuationSpelling P_hyphen_greater = "->"
punctuationSpelling P_paren_left     = "("
punctuationSpelling P_paren_right    = ")"
punctuationSpelling P_period         = "."
punctuationSpelling P_plus           = "+"
punctuationSpelling P_semicolon      = ";"
punctuationSpelling P_solidus        = "/"

-- |
-- Lex punctuation.
punctuation :: Punctuation -> Parser Position
punctuation = fmap fst . lexeme . Parser.try .
                Parser.string . Text.unpack . punctuationSpelling

--------------------------------------------------------------------------------
-- Literals

textLiteral :: Parser (Position, Text)
textLiteral = lexeme $ do
  _ <- Parser.char '"'
  cs <- many (Parser.noneOf ['"'])
  _ <- Parser.char '"'
  pure $ Text.pack cs

--------------------------------------------------------------------------------
-- Compound

-- |
-- Lex a name.
name :: Parser (Position, Name)
name = plainName <|> infixName <|> prefixName
  where
  plainName =
    fmap PlainName <$> identifier

  infixName = do
    position <- keyword K_infix
    infix_ <- Parser.choice [ -- Two characters.
                              InfixHyphenGreater <$ punctuation P_hyphen_greater

                              -- One character.
                            , InfixAsterisk <$ punctuation P_asterisk
                            , InfixHyphen <$ punctuation P_hyphen
                            , InfixPlus <$ punctuation P_plus
                            , InfixSolidus <$ punctuation P_solidus
                            ]
    pure (position, InfixName infix_)

  prefixName = do
    position <- keyword K_prefix
    prefix <- Parser.choice [ -- One character.
                              PrefixAsterisk <$ punctuation P_asterisk
                            ]
    pure (position, PrefixName prefix)

--------------------------------------------------------------------------------

lexeme :: Parser a -> Parser (Position, a)
lexeme parser = do
  _ <- many ignored
  position <- makePosition <$> Parser.getPosition
  result <- parser
  _ <- many ignored
  pure (position, result)
  where
  ignored = void space <|> void comment
  space   = Parser.oneOf " \t\r\n"
  comment = do { _ <- Parser.try (Parser.string "--")
               ; many (Parser.noneOf ['\n']) }

makePosition :: Parser.SourcePos -> Position
makePosition pos =
  let
    line   = Parser.sourceLine pos
    column = Parser.sourceColumn pos
    file   = Parser.sourceName pos
  in
    Position line column file
