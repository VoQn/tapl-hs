{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax where

import Data.Monoid
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

toRealNum :: NumVal -> Integer
toRealNum = walk 0
  where
  walk c nv = case nv of
    NumZero   -> c
    NumSucc v -> walk (c + 1) v

instance Ord NumVal where
  compare n1 n2 = toRealNum n1 `compare` toRealNum n2

instance Enum NumVal where
  fromEnum = fromInteger . toRealNum

  toEnum = walk NumZero
    where
    walk nv n
      | n > 0     = walk (NumSucc nv) $ n - 1
      | otherwise = nv

  pred nv = case nv of
    NumZero   -> NumZero
    NumSucc v -> v

instance Monoid NumVal where
  mempty = NumZero
  mappend nv1 nv2 = case (nv1, nv2) of
    (NumZero, NumZero)     -> NumZero
    (NumZero, _)           -> nv2
    (_, NumZero)           -> nv1
    (NumSucc v, NumSucc w) -> NumSucc $ NumSucc $ mappend v w

instance Display NumVal where
  toDisplay = toDisplay . fromEnum

instance Display Val where
  toDisplay v = case v of
    ValTrue   -> "true"
    ValFalse  -> "false"
    ValNum nv -> toDisplay nv
