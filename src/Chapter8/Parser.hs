
module Chapter8.Parser where

import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*))

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Info
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
      name = sourceName pos
    , line     = sourceLine pos
    , column   = sourceColumn pos
    }

pTrue :: Parser Term
pTrue = TmTrue <$> getInfo <* tReserved "true"

pFalse :: Parser Term
pFalse = TmFalse <$> getInfo <* tReserved "false"

pNat :: Parser Term
pNat = grow <$> getInfo <*> tNumber
  where
  grow i 0 = TmZero i
  grow i n = TmSucc i $ grow i $ n - 1

pSucc :: Parser Term
pSucc = tReserved "succ" >> (TmSucc <$> getInfo <*> pExpr)

pPred :: Parser Term
pPred = tReserved "pred" >> (TmPred <$> getInfo <*> pExpr)

pIsZero :: Parser Term
pIsZero = tReserved "zero?" >> (TmIsZero <$> getInfo <*> pExpr)

pIf :: Parser Term
pIf = tReserved "if" >> (TmIf <$> getInfo <*> pExpr <*> pExpr <*> pExpr)
