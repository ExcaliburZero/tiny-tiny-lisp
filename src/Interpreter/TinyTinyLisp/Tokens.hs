-- |
-- Module      : Interpreter.TinyTinyLisp.Tokens
-- Description : Contains lexical tokens.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
--
-- This module provides lexical tokens for Tiny Tiny Lisp.
module Interpreter.TinyTinyLisp.Tokens where

-- | A language token.
class Token a where
  -- | Shows the given token as a language string.
  showVal :: a -> String

-- | A token representing a value.
data ValueToken = EvaluationToken ExpressionToken
                | ValueLiteralToken LiteralToken
  deriving (Eq, Show)

-- | Converts the Value token into the contained value string.
instance Token ValueToken where
  showVal (EvaluationToken e)   = showVal e
  showVal (ValueLiteralToken l) = showVal l

-- | A token representing an expression.
data ExpressionToken = ExpressionToken FunctionToken [ValueToken]
  deriving (Eq, Show)

-- | Converts the ExpressionToken into the contained expression string.
instance Token ExpressionToken where
  showVal (ExpressionToken f vs) = "(" ++ showVal f ++ concatMap ((' ':) . showVal) vs ++ ")"

-- | A token representing a function.
data FunctionToken = FunctionOperatorToken OperatorToken
  deriving (Eq, Show)

-- | Converts the FunctionToken into the contained function string.
instance Token FunctionToken where
  showVal (FunctionOperatorToken t) = showVal t

-- | A token representing an operator.
data OperatorToken = PlusToken
                   | MinusToken
                   | MultiplyToken
                   | DivideToken
  deriving (Eq, Show)

-- | Converts the OperatorToken into the contained operator string.
instance Token OperatorToken where
  showVal PlusToken     = "+"
  showVal MinusToken    = "-"
  showVal MultiplyToken = "*"
  showVal DivideToken   = "/"

-- | A token representing a literal value.
data LiteralToken = LiteralNumberToken NumberToken
  deriving (Eq, Show)

-- | Converts the LiteralToken into the contained literal string.
instance Token LiteralToken where
  showVal (LiteralNumberToken x) = showVal x

-- | A token representing a number value.
data NumberToken = IntegerToken String | DecimalToken String
  deriving (Eq, Show)

-- | Converts the NumberToken into the contained number string.
instance Token NumberToken where
  showVal (IntegerToken x) = x
  showVal (DecimalToken x) = x
