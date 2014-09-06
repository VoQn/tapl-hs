{-# LANGUAGE LambdaCase #-}
module Chapter7.Syntax where

data Term
  = TmVar Int Int
  | TmAbs String Term
  | TmApp Term Term
  deriving (Eq, Show)

data Binding = NameBind

type Context = [(String, Binding)]

toDisplay :: Context -> Term -> String
toDisplay ctx = \case
  TmAbs x t ->
    let (ctx', x') = pickFreshName ctx x in
    "(\\" ++ x' ++ ". "　++ (toDisplay ctx' t) ++ ")"
  TmApp t1 t2 ->
    "(" ++ (toDisplay ctx t1) ++ " " ++ (toDisplay ctx t2) ++ ")"
  TmVar x n
    | length ctx == n -> indexToName ctx x
    | otherwise -> "[BAD INDEX]"

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

indexToName :: Context -> Int -> String
indexToName ctx x = let (xn, _) = ctx !! x in xn

nameToIndex :: Context -> String -> Int
nameToIndex ctx x = case ctx of
  [] -> error ("Identifier " ++ x ++ " is unbound")
  ((y,_):rest)
    | y == x -> 0
    | otherwise -> 1 + (nameToIndex rest x)
