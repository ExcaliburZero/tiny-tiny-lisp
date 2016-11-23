-- |
-- Module      : Interpreter.TinyTinyLisp.Parser
-- Description : Contains parsers and token evaluation functions.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
--
-- This module provides parsers for parsing Tiny Tiny Lisp, as well as
-- functions for evaluating lexical tokens.
module Interpreter.TinyTinyLisp.Parser where

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), empty, some)
import Safe (readMay)
import Text.ParserCombinators.Parsec (char, digit, Parser(..), try)
import Text.Parsec.Prim (parserFail, parserReturn)

import Interpreter.TinyTinyLisp.Lexer
import Interpreter.TinyTinyLisp.Tokens

-- $setup
-- >>> import Text.ParserCombinators.Parsec (parse)

-- | A value in Tiny Tiny Lisp.
type Value = Double

-- | A parser that attempts to parse and evaluate a value.
--
-- ==== __Examples__
--
-- >>> parse valueParser "" "(+ 1 2)"
-- Right 3.0
--
-- >>> parse valueParser "" "2.1"
-- Right 2.1
valueParser :: Parser Value
valueParser = do
  lex <- valueLexer
  case evaluateValue lex of
    Just v  -> parserReturn v
    Nothing -> parserFail   "Invalid value: "

-- | Attempts to evaluate the given ValueToken.
--
-- ==== __Examples__
--
-- >>> evaluateValue (EvaluationToken (ExpressionToken (FunctionOperatorToken PlusToken) [ValueLiteralToken (LiteralNumberToken (IntegerToken "1")),ValueLiteralToken (LiteralNumberToken (IntegerToken "2"))]))
-- Just 3.0
--
-- >>> evaluateValue (ValueLiteralToken (LiteralNumberToken (DecimalToken "5.1")))
-- Just 5.1
evaluateValue :: ValueToken -> Maybe Value
evaluateValue lex = case lex of
  EvaluationToken   t -> evaluateExpression t
  ValueLiteralToken t -> evaluateLiteral    t

-- | Attempts to evaluate the given ExpressionToken.
--
-- ==== __Examples__
--
-- >>> evaluateExpression (ExpressionToken (FunctionOperatorToken PlusToken) [ValueLiteralToken (LiteralNumberToken (IntegerToken "1"))])
-- Just 1.0
--
-- >>> evaluateExpression (ExpressionToken (FunctionOperatorToken MinusToken) [ValueLiteralToken (LiteralNumberToken (DecimalToken "2.1")),ValueLiteralToken (LiteralNumberToken (IntegerToken "1"))])
-- Just 1.1
evaluateExpression :: ExpressionToken -> Maybe Value
evaluateExpression (ExpressionToken fT vT) = do
  function <- evaluateFunction fT
  (x:xs)   <- mapM evaluateValue vT
  return $ foldl function x xs

-- | Attempts to evaluate the given FunctionToken.
evaluateFunction :: FunctionToken -> Maybe (Value -> Value -> Value)
evaluateFunction (FunctionOperatorToken t) = evaluateOperator t

-- | Attempts to evaluate the given OperatorToken.
evaluateOperator :: OperatorToken -> Maybe (Value -> Value -> Value)
evaluateOperator PlusToken     = Just (+)
evaluateOperator MinusToken    = Just (-)
evaluateOperator MultiplyToken = Just (*)
evaluateOperator DivideToken   = Just (/)

-- | Attempts to evaluate the given LiteralToken.
--
-- ==== __Examples__
--
-- >>> evaluateLiteral (LiteralNumberToken (IntegerToken "24"))
-- Just 24.0
evaluateLiteral :: LiteralToken -> Maybe Value
evaluateLiteral (LiteralNumberToken t) = evaluateNumber t

-- | Attempts to evaluate the given NumberToken.
--
-- ==== __Examples__
--
-- >>> evaluateNumber (IntegerToken "72")
-- Just 72.0
--
-- >>> evaluateNumber (DecimalToken "12.05")
-- Just 12.05
evaluateNumber :: NumberToken -> Maybe Value
evaluateNumber (IntegerToken t) = evaluateInteger t
evaluateNumber (DecimalToken t) = evaluateDecimal t

-- | Attempts to evaluate the given integer string
--
-- ==== __Examples__
--
-- >>> evaluateInteger "72"
-- Just 72.0
--
-- >>> evaluateInteger "the"
-- Nothing
evaluateInteger :: String -> Maybe Value
evaluateInteger = evaluateDecimal

-- | Attempts to evaluate the given decimal string.
--
-- ==== __Examples__
--
-- >>> evaluateDecimal "12.05"
-- Just 12.05
--
-- >>> evaluateDecimal "has"
-- Nothing
evaluateDecimal :: String -> Maybe Value
evaluateDecimal = readMay :: String -> Maybe Double
