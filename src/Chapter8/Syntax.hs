{-# LANGUAGE OverloadedStrings #-}
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
  toDisplay v = case v of
    ValTrue   -> "true"
    ValFalse  -> "false"
    ValNum nv -> toDisplay nv

instance HasType Val where
  typeof v = case v of
    ValTrue  -> Right TyBool
    ValFalse -> Right TyBool
    ValNum _ -> Right TyNat

requireType :: Ty -> Ty -> Term -> Either TypeError Ty
requireType req ret term = case typeof term of
  Right ty
    | ty == req -> Right ret
    | otherwise -> Left $ MismatchWithRequire req ty
  Left  err     -> Left err

requireSameType :: (String, Term) -> (String, Term) -> Either TypeError Ty
requireSameType (lt,tt) (lf,tf) = case (typeof tt, typeof tf) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right t, Right f)
    | t == f    -> Right t
    | otherwise -> Left $ MultiTypeReturn [(lt, t), (lf, f)]

instance HasType Term where
  typeof term = case term of
    TmTrue  -> Right TyBool
    TmFalse -> Right TyBool
    TmZero  -> Right TyNat

    TmIsZero t -> requireType TyNat TyBool t
    TmSucc   t -> requireType TyNat TyNat  t
    TmPred   t -> requireType TyNat TyNat  t

    TmIf p t f -> predCheck =<< restCheck
      where
        restCheck   = requireSameType ("then", t) ("else", f)
        predCheck r = requireType TyBool r p
