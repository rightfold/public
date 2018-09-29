module Granite.Organizational.Parse
  ( definition
  ) where

import Text.Parsec.Text (Parser)

import qualified Text.Parsec as Parser

import Granite.Behavioral.Abstract (pattern UniverseTypes, pattern UniverseValues)
import Granite.Behavioral.Parse (expression)
import Granite.Organizational.Abstract (Definition (..), DefinitionPayload (..))

import qualified Granite.Common.Lex as Lex

definition :: Parser Definition
definition = valueDefinition
  where
  valueDefinition :: Parser Definition
  valueDefinition = do
    position <- Lex.keyword Lex.K_value
    (_, name) <- Lex.name
    type_ <- Parser.optionMaybe $ Lex.keyword Lex.K_of *> expression UniverseTypes
    value <- Parser.optionMaybe $ Lex.keyword Lex.K_is *> expression UniverseValues
    _ <- Lex.punctuation Lex.P_semicolon
    pure $ Definition position (ValueDefinition name type_ value)
