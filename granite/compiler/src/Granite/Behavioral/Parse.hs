module Granite.Behavioral.Parse
  ( expression
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as Parser

import Granite.Behavioral.Abstract (pattern UniverseTypes, pattern UniverseValues, Builtin (..), Expression (..), ExpressionPayload (..), Universe)
import Granite.Common.Name (Infix (..), Name (..))

import qualified Granite.Common.Lex as Lex

expression :: Universe n -> Parser (Expression n)
expression = expression1

expression1 :: Universe n -> Parser (Expression n)
expression1 n = do
  left <- expression2 n
  next <- Parser.optionMaybe $
    (,) <$> Lex.punctuation Lex.P_hyphen_greater
        <*> expression1 n
  case next of
    Nothing -> pure left
    Just (arrowPosition, right) ->
      let arrow  = Expression arrowPosition (VariableExpression (InfixName InfixHyphenGreater)) in
      let first  = Expression (expressionPosition left) (ApplicationExpression arrow left) in
      let second = Expression (expressionPosition left) (ApplicationExpression first right) in
      pure second

expression2 :: Universe n -> Parser (Expression n)
expression2 n = do
  (function : arguments) <- Parser.many1 (expression3 n)
  let position = expressionPosition function
  let apply = (Expression position .) . ApplicationExpression
  pure $ foldl' apply function arguments

expression3 :: Universe n -> Parser (Expression n)
expression3 = \n -> case n of
  UniverseValues ->
    parenExpression n <|> variableExpression <|> lambdaExpression <|>
    builtinExpression <|> foreignExpression
  UniverseTypes ->
    parenExpression n <|> variableExpression <|> forallExpression
  _ ->
    parenExpression n <|> variableExpression
  where
  parenExpression :: Universe n -> Parser (Expression n)
  parenExpression n = do
    _ <- Lex.punctuation Lex.P_paren_left
    expr <- expression n
    _ <- Lex.punctuation Lex.P_paren_right
    pure expr

  variableExpression :: Parser (Expression n)
  variableExpression = do
    (position, name) <- Lex.name
    pure $ Expression position (VariableExpression name)

  lambdaExpression :: Parser (Expression 0)
  lambdaExpression = do
    position <- Lex.keyword Lex.K_lambda
    (_, parameter) <- Lex.name
    _ <- Lex.punctuation Lex.P_period
    body <- expression UniverseValues
    pure $ Expression position (LambdaExpression parameter body)

  builtinExpression :: Parser (Expression 0)
  builtinExpression = do
    (position, builtin) <-
      Parser.choice
        [ (, BuiltinAuxiliary    ) <$> Lex.keyword Lex.K___builtinAuxiliary
        , (, BuiltinAuxiliarySize) <$> Lex.keyword Lex.K___builtinAuxiliarySize
        , (, BuiltinCoerce       ) <$> Lex.keyword Lex.K___builtinCoerce
        , (, BuiltinEffJoin      ) <$> Lex.keyword Lex.K___builtinEffJoin
        , (, BuiltinEffMap       ) <$> Lex.keyword Lex.K___builtinEffMap
        , (, BuiltinEffPure      ) <$> Lex.keyword Lex.K___builtinEffPure ]
    pure $ Expression position (BuiltinExpression builtin)

  foreignExpression :: Parser (Expression 0)
  foreignExpression = do
    position <- Lex.keyword Lex.K_foreign
    (_, source) <- Lex.textLiteral
    _ <- Lex.keyword Lex.K_of
    type_ <- expression UniverseTypes
    pure $ Expression position (ForeignExpression source type_)

  forallExpression :: Parser (Expression 1)
  forallExpression = do
    position <- Lex.keyword Lex.K_forall
    (_, parameter) <- Lex.name
    _ <- Lex.punctuation Lex.P_period
    body <- expression UniverseTypes
    pure $ Expression position (ForallExpression parameter body)
