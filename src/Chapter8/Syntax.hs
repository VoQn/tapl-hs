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
