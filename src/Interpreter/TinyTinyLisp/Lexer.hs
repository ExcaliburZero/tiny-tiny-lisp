-- |
-- Module      : Interpreter.TinyTinyLisp.Lexer
-- Description : Contains parsers used for lexical analysis.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
--
-- This module provides parsers for lexing Tiny Tiny Lisp.
module Interpreter.TinyTinyLisp.Lexer where

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), empty, some)
import Text.ParserCombinators.Parsec (char, digit, Parser(..), try)

import Interpreter.TinyTinyLisp.Tokens

-- $setup
-- >>> import Text.ParserCombinators.Parsec (parse)

-- | A parser for lexing a value.
--
-- ==== __Examples__
--
-- >>> parse valueLexer "" "(+ 1 2)"
-- Right (EvaluationToken (ExpressionToken (FunctionOperatorToken PlusToken) [ValueLiteralToken (LiteralNumberToken (IntegerToken "1")),ValueLiteralToken (LiteralNumberToken (IntegerToken "2"))]))
--
-- >>> parse valueLexer "" "5.1"
-- Right (ValueLiteralToken (LiteralNumberToken (DecimalToken "5.1")))
valueLexer :: Parser ValueToken
valueLexer = EvaluationToken <$> (char '(' *> expressionLexer <* char ')')
         <|> ValueLiteralToken <$> literalLexer

-- | A parser for lexing an expression.
--
-- ==== __Examples__
--
-- >>> parse expressionLexer "" "+ 1"
-- Right (ExpressionToken (FunctionOperatorToken PlusToken) [ValueLiteralToken (LiteralNumberToken (IntegerToken "1"))])
--
-- >>> parse expressionLexer "" "- 2.1 1"
-- Right (ExpressionToken (FunctionOperatorToken MinusToken) [ValueLiteralToken (LiteralNumberToken (DecimalToken "2.1")),ValueLiteralToken (LiteralNumberToken (IntegerToken "1"))])
expressionLexer :: Parser ExpressionToken
expressionLexer = ExpressionToken <$> functionLexer <*> argumentsLexer

-- | A parser for lexing function arguments.
--
-- ==== __Examples__
--
-- >>> parse argumentsLexer "" " 1"
-- Right [ValueLiteralToken (LiteralNumberToken (IntegerToken "1"))]
--
-- >>> parse argumentsLexer "" " 2 7.3"
-- Right [ValueLiteralToken (LiteralNumberToken (IntegerToken "2")),ValueLiteralToken (LiteralNumberToken (DecimalToken "7.3"))]
argumentsLexer :: Parser [ValueToken]
argumentsLexer = some $ char ' ' *> valueLexer

-- | A parser for lexing a function.
--
-- ==== __Examples__
--
-- >>> parse functionLexer "" "-"
-- Right (FunctionOperatorToken MinusToken)
--
-- >>> parse functionLexer "" "*"
-- Right (FunctionOperatorToken MultiplyToken)
functionLexer :: Parser FunctionToken
functionLexer = FunctionOperatorToken <$> operatorLexer

-- | A parser for lexing an operator.
--
-- ==== __Examples__
--
-- >>> parse operatorLexer "" "+"
-- Right PlusToken
--
-- >>> parse operatorLexer "" "/"
-- Right DivideToken
operatorLexer :: Parser OperatorToken
operatorLexer = plusLexer
            <|> minusLexer
            <|> multiplyLexer
            <|> divideLexer

-- | A parser for lexing an addition operator.
--
-- ==== __Examples__
--
-- >>> parse plusLexer "" "+"
-- Right PlusToken
plusLexer :: Parser OperatorToken
plusLexer = const PlusToken <$> char '+'

-- | A parser for lexing a subtraction operator.
--
-- ==== __Examples__
--
-- >>> parse minusLexer "" "-"
-- Right MinusToken
minusLexer :: Parser OperatorToken
minusLexer = const MinusToken <$> char '-'

-- | A parser for lexing a multiplication operator.
--
-- ==== __Examples__
--
-- >>> parse multiplyLexer "" "*"
-- Right MultiplyToken
multiplyLexer :: Parser OperatorToken
multiplyLexer = const MultiplyToken <$> char '*'

-- | A parser for lexing a division operator.
--
-- ==== __Examples__
--
-- >>> parse divideLexer "" "/"
-- Right DivideToken
divideLexer :: Parser OperatorToken
divideLexer = const DivideToken <$> char '/'

-- | A parser for lexing a literal value.
--
-- ==== __Examples__
--
-- >>> parse literalLexer "" "24"
-- Right (LiteralNumberToken (IntegerToken "24"))
--
-- >>> parse literalLexer "" "sfn"
-- Left (line 1, column 1):
-- unexpected "s"
-- expecting digit
literalLexer :: Parser LiteralToken
literalLexer = LiteralNumberToken <$> numberLexer

-- | A parser for lexing a number value.
--
-- ==== __Examples__
--
-- >>> parse numberLexer "" "89"
-- Right (IntegerToken "89")
--
-- >>> parse numberLexer "" "72.03"
-- Right (DecimalToken "72.03")
--
-- >>> parse numberLexer "" "jsd"
-- Left (line 1, column 1):
-- unexpected "j"
-- expecting digit
numberLexer :: Parser NumberToken
numberLexer =  try decimalLexer <|> integerLexer

-- | A parser for lexing an integer value.
--
-- ==== __Examples__
--
-- >>> parse integerLexer "" "47"
-- Right (IntegerToken "47")
--
-- >>> parse integerLexer "" "abc"
-- Left (line 1, column 1):
-- unexpected "a"
-- expecting digit
integerLexer :: Parser NumberToken
integerLexer = IntegerToken <$> some digit

-- | A parser for lexing a decimal value.
--
-- ==== __Examples__
--
-- >>> parse decimalLexer "" "12.05"
-- Right (DecimalToken "12.05")
--
-- >>> parse decimalLexer "" "47"
-- Left (line 1, column 3):
-- unexpected end of input
-- expecting digit or "."
decimalLexer :: Parser NumberToken
decimalLexer = DecimalToken <$> do
  a <- some digit
  _ <- char '.'
  b <- some digit
  return $ a ++ "." ++ b
