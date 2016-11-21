module Interpreter.TinyTinyLisp.Lexer where

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), empty, some)
import Text.ParserCombinators.Parsec (char, digit, Parser(..), try)

import Interpreter.TinyTinyLisp.Tokens

valueLexer :: Parser ValueToken
valueLexer = EvaluationToken <$> (char '(' *> expressionLexer <* char ')')
         <|> ValueLiteralToken <$> literalLexer

expressionLexer :: Parser ExpressionToken
expressionLexer = ExpressionToken <$> functionLexer <*> argumentsLexer

argumentsLexer :: Parser [ValueToken]
argumentsLexer = some $ char ' ' *> valueLexer

functionLexer :: Parser FunctionToken
functionLexer = FunctionOperatorToken <$> operatorLexer

operatorLexer :: Parser OperatorToken
operatorLexer = plusLexer
            <|> minusLexer
            <|> multiplyLexer
            <|> divideLexer

plusLexer :: Parser OperatorToken
plusLexer = const PlusToken <$> char '+'

minusLexer :: Parser OperatorToken
minusLexer = const MinusToken <$> char '-'

multiplyLexer :: Parser OperatorToken
multiplyLexer = const MultiplyToken <$> char '*'

divideLexer :: Parser OperatorToken
divideLexer = const DivideToken <$> char '/'

literalLexer :: Parser LiteralToken
literalLexer = LiteralNumberToken <$> numberLexer

numberLexer :: Parser NumberToken
numberLexer =  try decimalLexer <|> integerLexer

integerLexer :: Parser NumberToken
integerLexer = IntegerToken <$> some digit

decimalLexer :: Parser NumberToken
decimalLexer = DecimalToken <$> do
  a <- some digit
  _ <- char '.'
  b <- some digit
  return $ a ++ "." ++ b
