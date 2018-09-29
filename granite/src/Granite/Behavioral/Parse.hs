module Granite.Behavioral.Parse
  ( expression
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as Parser

import Granite.Behavioral.Abstract (pattern UniverseTypes, pattern UniverseValues, Expression (..), ExpressionPayload (..), Universe)

import qualified Granite.Common.Lex as Lex

expression :: Universe n -> Parser (Expression n)
expression = expression1

expression1 :: Universe n -> Parser (Expression n)
expression1 n = do
  (function : arguments) <- Parser.many1 (expression2 n)
  let position = expressionPosition function
  let apply = (Expression position .) . ApplicationExpression
  pure $ foldl' apply function arguments

expression2 :: Universe n -> Parser (Expression n)
expression2 = \n -> case n of
  UniverseValues ->
    parenExpression n <|> variableExpression <|> lambdaExpression <|>
    foreignExpression
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

  foreignExpression :: Parser (Expression 0)
  foreignExpression = do
    position <- Lex.keyword Lex.K_foreign
    (_, source) <- Lex.textLiteral
    pure $ Expression position (ForeignExpression source)

  forallExpression :: Parser (Expression 1)
  forallExpression = do
    position <- Lex.keyword Lex.K_forall
    (_, parameter) <- Lex.name
    _ <- Lex.punctuation Lex.P_period
    body <- expression UniverseTypes
    pure $ Expression position (ForallExpression parameter body)
