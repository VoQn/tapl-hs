module Chapter7.Parser where

import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as Map
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
  ctx <- mkCtx [] <$> vars
  bdy <- bodyExpr ctx <$> vars
  return $ ctx `apply` bdy
  where
  vars :: Parser [Name]
  vars = whitespace $ cplx <||> smpl
  cplx = parens $ many1 $ whitespace tId
  smpl = (:[]) <$> tId

  mkCtx :: Context -> [Name] -> Context
  mkCtx rs [] = rs
  mkCtx rs (x:xs) = mkCtx ((x, NameBind) : rs) xs

  bodyExpr :: Context -> [Name] -> Term
  bodyExpr ctx es =
    let l = length ctx in
    case mapM (lkup ctx l) es of
      Just xs -> foldl1 (<+>) xs
      Nothing -> undefined

  lkup :: Context -> Int -> Name -> Maybe Term
  lkup ctx l e = case nameToIndex ctx e of
    Right i -> Just $ i <+ l
    Left  _ -> Map.lookup e builtIns

  apply :: Context -> Term -> Term
  apply ctx e = foldr (\l r -> (fst l) +> r) e (reverse ctx)


tId :: Parser Name
tId = (:) <$> letter <*> many alphaNum

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
