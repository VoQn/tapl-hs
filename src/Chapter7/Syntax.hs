{-# LANGUAGE LambdaCase,OverloadedStrings #-}
module Chapter7.Syntax where

import Control.Applicative
import Data.Display
import qualified Data.Text.Lazy.Builder as LB

data Term
  = TmVar Int Int     -- TmVar { index :: Int, contextLength :: Int }
  | TmAbs String Term
  | TmApp Term Term
  deriving (Eq, Show)

data Binding
  = NameBind
  deriving (Eq, Show)

type Context = [(String, Binding)]
type ErrorMsg = String

withContext :: Context -> Term -> LB.Builder
withContext ctx = \case

  TmAbs x t ->
    let (ctx', x') = pickFreshName ctx x
        x''        = toDisplay x'
        desc       = toDisplay $ withContext ctx' t
    in　parens $ spaceSep $ ["\\", x'', desc]

  TmApp t1 t2 ->
    let disp = toDisplay . withContext ctx in
    parens $ spaceSep $ map disp [t1, t2]

  TmVar x n
    | length ctx == n ->
        either toDisplay toDisplay $ indexToName ctx x
    | otherwise ->
        "[BAD INDEX]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = (((x, NameBind) : ctx), x)

isNameBound :: Context -> String -> Bool
isNameBound ctx x = case ctx of
  [] -> False
  ((y,_):rest)
    | y == x -> True
    | otherwise -> isNameBound rest x

indexToName :: Context -> Int -> Either ErrorMsg String
indexToName ctx x
  | length ctx <= x =
      Left $ "Not found indexed-" ++ show x ++ " variable in this context"
  | otherwise =
      Right $ fst $ ctx !! x

nameToIndex :: Context -> String -> Either ErrorMsg Int
nameToIndex ctx x = case ctx of
  [] -> Left $ "Identifier " ++ x ++ " is unbound"
  ((y,_):rest)
    | y == x -> Right 0
    | otherwise -> (1 +) <$> nameToIndex rest x
