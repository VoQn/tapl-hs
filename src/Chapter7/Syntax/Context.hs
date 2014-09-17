module Chapter7.Syntax.Context where

import Chapter7.Syntax.Term
-------------------------------------------------------------------------------
-- Context (Varibale's Name)
-------------------------------------------------------------------------------
type Context = [Name]

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName c n
  | isNameBound c n = pickFreshName c (n ++ "'")
  | otherwise       = ((n : c), n)

isNameBound :: Context -> Name -> Bool
isNameBound c n = case c of
  [] -> False
  (y:ys)
    | y == n -> True
    | otherwise  -> isNameBound ys n

(+:+) :: Context -> [Name] -> Context
(+:+) cx xs = case xs of
  []     -> cx
  (n:ns) -> (n:cx) +:+ ns
