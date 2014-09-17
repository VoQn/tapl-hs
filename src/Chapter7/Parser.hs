module Chapter7.Parser where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String (Parser)

import Chapter7.Identifier
import Chapter7.Syntax

whitespace :: Parser a -> Parser a
whitespace p = many space *> p

parens :: Parser a -> Parser a
parens p = char '(' *> whitespace p <* whitespace (char ')')

parseExpr :: String -> Either ParseError Term
parseExpr = parse (pContents $ pExpr []) "<stdin>"

pContents :: Parser a -> Parser a
pContents p = many space *> p <* eof

pExpr :: Context -> Parser Term
pExpr ctx = whitespace $ pTerm ctx

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) p1 p2 = try p1 <|> p2

pTerm :: Context -> Parser Term
pTerm ctx
  =    pVar ctx
  <||> pApp ctx
  <||> pAbs ctx

tId :: Parser Name
tId = (:) <$> letter <*> many (alphaNum <||> tSym)

tSym :: Parser Char
tSym = oneOf "+-?:$#<>"

pVar :: Context -> Parser Term
pVar ctx = do
  let l = length ctx
  n <- tId
  return $ case nameToIndex ctx n of
    Right i -> i <+ l
    Left  _ -> TmFree n

manyVar :: Parser [Name]
manyVar = parens $ many1 $ whitespace tId

uniqVar :: Parser [Name]
uniqVar = (:[]) <$> tId

-- |
-- Parse Lambda Expression
-- (\\ x x) => λx.x (λ.0)
-- (\\ (x y) x) => λxy.x (λ.1)
-- (\\ (x y z a) x) => λxyza.x (λ.3)
-- (\\ (f g) (g f)) => λfg.(g f) (λ.0 1)
pAbs :: Context -> Parser Term
pAbs ctx = parens form <?> "Lambda Expression (ex: (\\ (x y ...) (x ...))"
  where
  form :: Parser Term
  form = do
    _   <- char '\\'
    ags <- whitespace $ manyVar <||> uniqVar
    bdy <- pExpr $ ctx +:+ ags
    return $ foldr (+>) bdy ags

pApp :: Context -> Parser Term
pApp ctx = parens form <?> "Function Apply (ex: (f x))"
  where
  form :: Parser Term
  form = foldl1 (<+>) <$> many (pExpr ctx)
