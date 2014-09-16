module Chapter7.Syntax.Context where

import Chapter7.Syntax.Term
-------------------------------------------------------------------------------
-- Context (Varibale's Name : State)
-------------------------------------------------------------------------------
data Binding = NameBind deriving (Eq, Show)

type Context = [(Name, Binding)]

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName c n
  | isNameBound c n = pickFreshName c (n ++ "'")
  | otherwise       = (((n, NameBind) : c), n)

isNameBound :: Context -> Name -> Bool
isNameBound c n = case c of
  [] -> False
  (y:ys)
    | fst y == n -> True
    | otherwise  -> isNameBound ys n

(+:+) :: Context -> [Name] -> Context
(+:+) cx xs = case xs of
  []     -> cx
  (n:ns) -> ((n, NameBind):cx) +:+ ns
