{-# LANGUAGE LambdaCase #-}
module Chapter7.Syntax.Term where

type Name = String

data Term
  = TmVar Int Int     -- TmVar { index :: Int, depth :: Int }
  | TmAbs Name Term
  | TmApp Term Term
  deriving (Eq, Show)

data Binding
  = NameBind
  deriving (Eq, Show)

(<+) :: Int -> Int -> Term
i <+ l = TmVar i l
infix 4 <+

(+>) :: Name -> Term -> Term
n +> t = TmAbs n t
infixr 2 +>

(<+>) :: Term -> Term -> Term
t1 <+> t2 = TmApp t1 t2
infixl 3 <+>

-------------------------------------------------------------------------------
-- Shift (Modify Variable's Index & Context Depth)
-------------------------------------------------------------------------------

shift :: Int -> Term -> Term
shift d = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    TmAbs x t   -> x +> walk (c + 1) t
    TmApp t1 t2 -> walk c t1 <+> walk c t2
    TmVar i l
      | i >= l    -> i + d <+ l + d
      | otherwise -> i <+ l + d

(-^) :: Int -> Term -> Term
d -^ t = shift d t
infix 9 -^

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
  where
  walk :: Int -> Term -> Term
  walk c = \case
    TmAbs x t   -> x +> walk (c + 1) t
    TmApp t1 t2 -> walk c t1 <+> walk c t2
    TmVar i l
      | i == j + c -> c -^ s
      | otherwise  -> i <+ l

substTop :: Term -> Term -> Term
substTop s =
  ((-1) -^) . subst 0 (1 -^ s)
