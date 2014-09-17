{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter7.Syntax where

import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy.Builder as LB

import Data.Display

import Chapter7.Identifier
import Chapter7.Exception

data Term
  = TmFree Name
  | TmVar Int Int     -- TmVar { index :: Int, depth :: Int }
  | TmAbs Name Term
  | TmApp Term Term
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Alias for data constructor of `Term`
-------------------------------------------------------------------------------

(<+) :: Int -> Int -> Term
(<+) = TmVar
infix 4 <+

(+>) :: Name -> Term -> Term
(+>) = TmAbs
infixr 2 +>

(<+>) :: Term -> Term -> Term
(<+>) = TmApp
infixl 3 <+>

-------------------------------------------------------------------------------
-- Shift (Modify Variable's Index & Context Depth)
-------------------------------------------------------------------------------

shift :: Int -> Term -> Term
shift d = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    v@(TmFree _) -> v
    TmAbs x t    -> x +> walk (c + 1) t
    TmApp t1 t2  -> walk c t1 <+> walk c t2
    TmVar i l
      | i >= c    -> i + d <+ l + d
      | otherwise -> i <+ l + d

(-^) :: Int -> Term -> Term
(-^) = shift
infix 9 -^

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    v@(TmFree _) -> v
    TmAbs x t    -> x +> walk (c + 1) t
    TmApp t1 t2  -> walk c t1 <+> walk c t2
    TmVar i l
      | i == j + c -> c -^ s
      | otherwise  -> i <+ l

substTop :: Term -> Term -> Term
substTop s = ((-1) -^) . subst 0 (1 -^ s)

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
  []             -> False
  (y:ys)
    | y == n     -> True
    | otherwise  -> isNameBound ys n

(+:+) :: Context -> [Name] -> Context
(+:+) cx = \case
  []     -> cx
  (n:ns) -> (n:cx) +:+ ns

-------------------------------------------------------------------------------
-- Term Control
-------------------------------------------------------------------------------

indexToName :: Context -> Int -> Int -> Either RuntimeException Name
indexToName c i l
  | length c /= l = Left $ WrongContextDepth i l $ length c
  | length c <= i = Left $ OutOfContextIndex i
  | otherwise     = Right $ c !! i

nameToIndex :: Context -> Name -> Either RuntimeException Int
nameToIndex c n = case c of
  []            -> Left $ UnboundIdentifier n
  (y:ys)
    | y == n    -> Right 0
    | otherwise -> (1 +) <$> nameToIndex ys n

-------------------------------------------------------------------------------
-- Display Term & Exception
-------------------------------------------------------------------------------
instance Display Term where
  toDisplay = withContext []

withContext :: Context -> Term -> LB.Builder
withContext c = \case
  TmApp t1 t2
    -> "(" <> spaceSep (map (disp c) [t1,t2]) <> ")"
  TmVar i l
    -> eitherDisplay $ indexToName c i l
  TmFree n
    -> toDisplay n
  TmAbs n t
    -> let (c', n') = pickFreshName c n in
       "(" <> spaceSep ["\\", toDisplay n', disp c' t] <> ")"
  where
  disp :: Context -> Term -> LB.Builder
  disp ctx = toDisplay . withContext ctx
