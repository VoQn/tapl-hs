{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax where

import Data.Display

import Chapter8.NumVal

data Ty
  = TyBool
  | TyNat
  deriving (Eq, Show)

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

instance Display Ty where
  toDisplay ty = case ty of
    TyBool -> "Bool"
    TyNat  -> "Nat"

instance Display Val where
  toDisplay v = case v of
    ValTrue   -> "true"
    ValFalse  -> "false"
    ValNum nv -> toDisplay nv
