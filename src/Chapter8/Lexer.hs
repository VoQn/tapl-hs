module Chapter8.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ emptyDef {
    Token.commentLine     = ";"
  , Token.reservedNames   = keywords
  , Token.reservedOpNames = operators
  }
  where
  keywords  = ["true", "false", "succ", "pred", "zero?", "if"]
  operators = [".", "$"]

tSpace :: Parser ()
tSpace = Token.whiteSpace lexer

tParens :: Parser a -> Parser a
tParens = Token.parens lexer

tNumber :: Parser Integer
tNumber = Token.natural lexer

tLexeme :: Parser a -> Parser a
tLexeme = Token.lexeme lexer

tReserved :: String -> Parser ()
tReserved = Token.reserved lexer

tReservedOp :: String -> Parser ()
tReservedOp = Token.reservedOp lexer
