{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax.Term where

import Data.Display

import Chapter8.Syntax.Type

data Term
  = TmTrue                  -- ^ true
  | TmFalse                 -- ^ false
  | TmZero                  -- ^ 0
  | TmSucc   Term           -- ^ succ <term>
  | TmPred   Term           -- ^ pred <term>
  | TmIsZero Term           -- ^ zero? <term>
  | TmIf     Term Term Term -- ^ if <term> then <term> else <term>
  deriving (Eq, Show)

requireType :: Ty -> Ty -> Term -> Either TypeError Ty
requireType req ret term = do
  ty <- typeof term
  if ty == req
    then Right ret
    else Left $ MismatchWithRequire req ty

requireSameType :: (String, Term) -> (String, Term) -> Either TypeError Ty
requireSameType (lt,tt) (lf,tf) = do
  t <- typeof tt
  f <- typeof tf
  if t == f
    then Right t
    else Left $ MultiTypeReturn [(lt, t), (lf, f)]

instance Display Term where
  toDisplay term = case term of
    TmTrue  -> "true"
    TmFalse -> "false"
    TmZero  -> "0"

    TmSucc   t -> dispSucc 1 t
    TmPred   t -> dispApp "pred"  [t]
    TmIsZero t -> dispApp "zero?" [t]

    TmIf p t f -> dispApp "if"    [p,t,f]
    where
    dispApp  f = parens . spaceSep . (f :) . map toDisplay
    dispSucc n tm = case tm of
      TmZero   -> toDisplay $ n `max` (0 :: Integer)
      TmSucc t -> dispSucc (n + 1) t
      others   -> dispApp "succ" [others]

instance HasType Term where
  typeof term = case term of
    TmTrue  -> Right TyBool
    TmFalse -> Right TyBool
    TmZero  -> Right TyNat

    TmSucc   t -> requireType TyNat TyNat  t
    TmPred   t -> requireType TyNat TyNat  t
    TmIsZero t -> requireType TyNat TyBool t

    TmIf p t f -> predCheck =<< retnCheck
      where
      retnCheck   = requireSameType ("then", t) ("else", f)
      predCheck r = requireType TyBool r p
