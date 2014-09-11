module Chapter7.Parser where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String (Parser)

import Chapter7.Syntax

whitespace :: Parser a -> Parser a
whitespace p = many space *> p

parens :: Parser a -> Parser a
parens p = char '(' *> whitespace p <* whitespace (char ')')

parseExpr :: String -> Either ParseError Term
parseExpr = parse (pContents pExpr) "<stdin>"

pContents :: Parser a -> Parser a
pContents p = many space *> p <* eof

pCall :: String -> Parser a -> Parser a
pCall fn p = parens $ string fn *> p

pExpr :: Parser Term
pExpr = whitespace pTerm

p1 <||> p2 = try p1 <|> p2

pTerm :: Parser Term
pTerm
  =    pId
  <||> pTru
  <||> pFls
  <||> pTest
  <||> pAnd
  <||> pOr
  <||> pApp

pId :: Parser Term
pId = cId <$ string "id"

pTru :: Parser Term
pTru = cTru <$ string "tru"

pFls :: Parser Term
pFls = cFls <$ string "fls"

pTest :: Parser Term
pTest = cTest <$ string "test"

pAnd :: Parser Term
pAnd = cAnd <$ string "and"

pOr :: Parser Term
pOr = cOr <$ string "or"

pApp :: Parser Term
pApp = parens $ foldl1 (<+>) <$> many pExpr
