
module Chapter8.Parser where

import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*))

import Text.Parsec
import Text.Parsec.String (Parser)

import Chapter8.Info
import Chapter8.Syntax
import Chapter8.Lexer

parseTerm :: String -> Either ParseError Term
parseTerm = parse (pContents pExpr) "<stdin>"

pContents :: Parser a -> Parser a
pContents p = tSpace *> p <* eof

pExpr :: Parser Term
pExpr = tSpace *> pTerm

pTerm :: Parser Term
pTerm
  =    pTrue
  <||> pFalse
  <||> pNat
  <||> pSucc
  <||> pPred
  <||> pIsZero
  <||> pIf
  <||> tParens pTerm

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) p q = try p <|> q

getInfo :: Parser Info
getInfo = do
  pos <- getPosition
  return $ FileImput {
      fileName = sourceName pos
    , line     = sourceLine pos
    , column   = sourceColumn pos
    }

pTrue :: Parser Term
pTrue = TmTrue <$ tReserved "true"

pFalse :: Parser Term
pFalse = TmFalse <$ tReserved "false"

pNat :: Parser Term
pNat = grow <$> tNumber
  where
  grow 0 = TmZero
  grow n = TmSucc $ grow (n - 1)

pSucc :: Parser Term
pSucc = tReserved "succ" >> (TmSucc <$> pExpr)

pPred :: Parser Term
pPred = tReserved "pred" >> (TmPred <$> pExpr)

pIsZero :: Parser Term
pIsZero = tReserved "zero?" >> (TmIsZero <$>  pExpr)

pIf :: Parser Term
pIf = tReserved "if" >> (TmIf <$> pExpr <*> pExpr <*> pExpr)
