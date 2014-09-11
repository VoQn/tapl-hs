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

pExpr :: Parser Term
pExpr = whitespace pTerm

(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = try p1 <|> p2

pTerm :: Parser Term
pTerm
  =    pId
  <||> pTru
  <||> pFls
  <||> pTst
  <||> pAnd
  <||> pOr
  <||> pApp

pId :: Parser Term
pId = cId <$ string "id"

pTru :: Parser Term
pTru = cTru <$ string "tru"

pFls :: Parser Term
pFls = cFls <$ string "fls"

pTst :: Parser Term
pTst = cTst <$ string "tst"

pAnd :: Parser Term
pAnd = cAnd <$ string "and"

pOr :: Parser Term
pOr = cOr <$ string "or"

pApp :: Parser Term
pApp = parens $ foldl1 (<+>) <$> many pExpr
