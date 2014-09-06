{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter4.Syntax where

import Chapter4.Display

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

data Ty
  = TyBool
  | TyNat
  deriving (Eq, Show)

instance Display Term where
  toDisplay = \case
    TmTrue  -> "true"
    TmFalse -> "false"
    TmZero  -> "0"
    TmIf p t f -> parens $ spaceSep $ "if"    : map toDisplay [p,t,f]
    TmSucc   t -> parens $ spaceSep $ "succ"  : [toDisplay t]
    TmPred   t -> parens $ spaceSep $ "pred"  : [toDisplay t]
    TmIsZero t -> parens $ spaceSep $ "zero?" : [toDisplay t]

instance Display Ty where
  toDisplay = \case
    TyBool -> "Bool"
    TyNat  -> "Nat"

isNumeric :: Term -> Bool
isNumeric = \case
  TmZero   -> True
  TmSucc t -> isNumeric t
  TmPred t -> isNumeric t
  TmIf p t f -> isBool p && all isNumeric [t,f]
  _ -> False

isBool :: Term -> Bool
isBool = \case
  TmTrue  -> True
  TmFalse -> True
  TmIsZero t -> isNumeric t
  TmIf p t f -> all isBool [p,t,f]
  _ -> False
