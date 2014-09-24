{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax.Term where

import Data.Display

import Chapter8.Info
import Chapter8.Syntax.Type

data Term
  = TmTrue   Info                -- ^ true
  | TmFalse  Info                -- ^ false
  | TmZero   Info                -- ^ 0
  | TmSucc   Info Term           -- ^ succ <term>
  | TmPred   Info Term           -- ^ pred <term>
  | TmIsZero Info Term           -- ^ zero? <term>
  | TmIf     Info Term Term Term -- ^ if <term> then <term> else <term>
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
    TmTrue  _ -> "true"
    TmFalse _ -> "false"
    TmZero  _ -> "0"

    TmSucc   _ t -> dispSucc 1 t
    TmPred   _ t -> dispApp "pred"  [t]
    TmIsZero _ t -> dispApp "zero?" [t]

    TmIf _ p t f -> dispApp "if"    [p,t,f]
    where
    dispApp  f = parens . spaceSep . (f :) . map toDisplay
    dispSucc n tm = case tm of
      TmZero _   -> toDisplay $ n `max` (0 :: Integer)
      TmSucc _ t -> dispSucc (n + 1) t
      others     -> dispApp "succ" [others]

instance HasType Term where
  typeof term = case term of
    TmTrue  _ -> Right TyBool
    TmFalse _ -> Right TyBool
    TmZero  _ -> Right TyNat

    TmSucc   _ t -> requireType TyNat TyNat  t
    TmPred   _ t -> requireType TyNat TyNat  t
    TmIsZero _ t -> requireType TyNat TyBool t

    TmIf _ p t f -> predCheck =<< retnCheck
      where
      retnCheck   = requireSameType ("then", t) ("else", f)
      predCheck r = requireType TyBool r p
