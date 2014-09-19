{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter8.Syntax where

import Data.Display

import Chapter8.Type
import Chapter8.NumVal

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

data Val
  = ValTrue
  | ValFalse
  | ValNum NumVal
  deriving (Eq, Show)

instance Display Val where
  toDisplay = \case
    ValTrue  -> "true"
    ValFalse -> "false"
    ValNum v -> toDisplay v

instance HasType Val where
  typeof = \case
    ValTrue  -> Right TyBool
    ValFalse -> Right TyBool
    ValNum _ -> Right TyNat

requireType :: Ty -> Ty -> Term -> Either TypeError Ty
requireType req ret term = case typeof term of
  Left  err -> Left err
  Right ty
    | ty == req -> Right ret
    | otherwise -> Left $ MismatchWithRequire req ty

requireSameType :: (String, Term) -> (String, Term) -> Either TypeError Ty
requireSameType (lt,tt) (lf,tf) = case (typeof tt, typeof tf) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right t, Right f)
    | t == f    -> Right t
    | otherwise -> Left $ MultiTypeReturn [(lt, t), (lf, f)]

instance Display Term where
  toDisplay = \case
    TmTrue  -> "true"
    TmFalse -> "false"
    TmZero  -> "0"

    TmSucc   t -> dispSucc 1 t
    TmPred   t -> dispApp "pred"  [t]
    TmIsZero t -> dispApp "zero?" [t]

    TmIf p t f -> dispApp "if"    [p,t,f]
    where
    dispApp  f = parens . spaceSep . (f :) . map toDisplay
    dispSucc n = \case
      TmZero   -> toDisplay $ n `max` (0 :: Integer)
      TmSucc t -> dispSucc (n + 1) t
      others   -> dispApp "succ" [others]

instance HasType Term where
  typeof = \case
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
