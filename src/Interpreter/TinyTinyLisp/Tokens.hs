module Interpreter.TinyTinyLisp.Tokens where

class Token a where
  showVal :: a -> String

data ValueToken = EvaluationToken ExpressionToken
                | ValueLiteralToken LiteralToken
  deriving (Eq, Show)

instance Token ValueToken where
  showVal (EvaluationToken e)   = showVal e
  showVal (ValueLiteralToken l) = showVal l

data ExpressionToken = ExpressionToken FunctionToken [ValueToken]
  deriving (Eq, Show)

instance Token ExpressionToken where
  showVal (ExpressionToken f vs) = "(" ++ showVal f ++ concatMap ((' ':) . showVal) vs ++ ")"

data FunctionToken = FunctionOperatorToken OperatorToken
  deriving (Eq, Show)

instance Token FunctionToken where
  showVal (FunctionOperatorToken t) = showVal t

data OperatorToken = PlusToken
                   | MinusToken
                   | MultiplyToken
                   | DivideToken
  deriving (Eq, Show)

instance Token OperatorToken where
  showVal PlusToken     = "+"
  showVal MinusToken    = "-"
  showVal MultiplyToken = "*"
  showVal DivideToken   = "/"

data LiteralToken = LiteralNumberToken NumberToken
  deriving (Eq, Show)

instance Token LiteralToken where
  showVal (LiteralNumberToken x) = showVal x

data NumberToken = IntegerToken String | DecimalToken String
  deriving (Eq, Show)

instance Token NumberToken where
  showVal (IntegerToken x) = x
  showVal (DecimalToken x) = x
