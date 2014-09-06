module Chapter4.Parser (parseExpr) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String (Parser)

import Chapter4.Syntax

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
pExpr = whitespace $ pTerm

pTerm :: Parser Term
pTerm
   =  try pTrue
  <|> try pFalse
  <|> try pZero
  <|> try pIf
  <|> try pSucc
  <|> try pPred
  <|> try pIsZero
  <|> (parens pTerm)

pTrue :: Parser Term
pTrue = TmTrue <$ string "true"

pFalse :: Parser Term
pFalse = TmFalse <$ string "false"

pZero :: Parser Term
pZero = TmZero <$ char '0'

pIf :: Parser Term
pIf = pCall "if" $ TmIf <$> pExpr <*> pExpr <*> pExpr

pSucc :: Parser Term
pSucc = pCall "succ" $ TmSucc <$> pExpr

pPred :: Parser Term
pPred = pCall "pred" $ TmPred <$> pExpr

pIsZero :: Parser Term
pIsZero = pCall "zero?" $ TmIsZero <$> pExpr
