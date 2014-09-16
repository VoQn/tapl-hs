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
  =    pLam
  <||> pId
  <||> pTru
  <||> pFls
  <||> pTst
  <||> pAnd
  <||> pOr
  <||> pApp

-- |
-- Parse Lambda Expression
-- (\\ x x) => λx.x (λ.0)
-- (\\ (x y) x) => λxy.x (λ.1)
-- (\\ (x y z a) x) => λxyza.x (λ.3)
-- (\\ (f g) (g f)) => λfg.(g f) (λ.0 1)
pLam :: Parser Term
pLam = parens $ do
  _   <- char '\\'
  ags <- whitespace $ cplx <||> smpl
  let ctx = mkCtx ags
  bdy <- whitespace $ pApp' ctx <||> pVar ctx
  return $ foldr (+>) bdy ags
  where
  cplx :: Parser [Name]
  cplx = parens $ many1 $ whitespace tId

  smpl :: Parser [Name]
  smpl = (:[]) <$> tId

  mkCtx :: [Name] -> Context
  mkCtx = map (\x -> (x, NameBind)) . reverse

pVar :: Context -> Parser Term
pVar ctx = do
  let l = length ctx
  n <- tId
  return $ case nameToIndex ctx n of
    Right i -> i <+ l
    Left  _ -> TmFree n

pApp' :: Context -> Parser Term
pApp' ctx = foldl1 (<+>) <$> parens vars
  where
  vars = many $ whitespace $ pApp' ctx <||> pVar ctx

tId :: Parser Name
tId = (:) <$> letter <*> many (alphaNum <||> tSym)

tSym :: Parser Char
tSym = oneOf "+-?:$#<>"

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
