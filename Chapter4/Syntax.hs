{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Syntax where

import Data.List (intercalate)
import qualified Data.Text.Lazy.Builder as LB
import Display

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Ord, Show)

data Ty
  = TyBool
  | TyNat
  deriving (Eq, Ord, Show)

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

isVal :: Term -> Bool
isVal t = case t of
  TmTrue  -> True
  TmFalse -> True
  _ -> isNumeric t
