module Interpreter.TinyTinyLisp.Parser where

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), empty, some)
import Safe (readMay)
import Text.ParserCombinators.Parsec (char, digit, Parser(..), try)
import Text.Parsec.Prim (parserFail, parserReturn)

import Interpreter.TinyTinyLisp.Lexer
import Interpreter.TinyTinyLisp.Tokens

type Value = Double

valueParser :: Parser Value
valueParser = do
  lex <- valueLexer
  case evaluateValue lex of
    Just v  -> parserReturn v
    Nothing -> parserFail   "Invalid value: "

evaluateValue :: ValueToken -> Maybe Value
evaluateValue lex = case lex of
  EvaluationToken   t -> evaluateExpression t
  ValueLiteralToken t -> evaluateLiteral    t

evaluateExpression :: ExpressionToken -> Maybe Value
evaluateExpression (ExpressionToken fT vT) = do
  function <- evaluateFunction fT
  (x:xs)   <- mapM evaluateValue vT
  return $ foldl function x xs

evaluateFunction :: FunctionToken -> Maybe (Value -> Value -> Value)
evaluateFunction (FunctionOperatorToken t) = evaluateOperator t

evaluateOperator :: OperatorToken -> Maybe (Value -> Value -> Value)
evaluateOperator PlusToken     = Just (+)
evaluateOperator MinusToken    = Just (-)
evaluateOperator MultiplyToken = Just (*)
evaluateOperator DivideToken   = Just (/)

evaluateLiteral :: LiteralToken -> Maybe Value
evaluateLiteral (LiteralNumberToken t) = evaluateNumber t

evaluateNumber :: NumberToken -> Maybe Value
evaluateNumber (IntegerToken t) = evaluateInteger t
evaluateNumber (DecimalToken t) = evaluateDecimal t

evaluateInteger :: String -> Maybe Value
evaluateInteger = evaluateDecimal

evaluateDecimal :: String -> Maybe Value
evaluateDecimal = readMay :: String -> Maybe Double
