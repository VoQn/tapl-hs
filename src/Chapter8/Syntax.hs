{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax where

import Data.Display

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

data NumVal
  = NumZero
  | NumSucc NumVal
  deriving (Eq, Show)

instance Display Ty where
  toDisplay ty = case ty of
    TyBool -> "Bool"
    TyNat  -> "Nat"

instance Display NumVal where
  toDisplay = toDisplay . realNum

realNum :: NumVal -> Integer
realNum = realNum' 0
  where
  realNum' c nv = case nv of
    NumZero   -> c
    NumSucc v -> realNum' (c + 1) v

instance Display Val where
  toDisplay v = case v of
    ValTrue   -> "true"
    ValFalse  -> "false"
    ValNum nv -> toDisplay nv
