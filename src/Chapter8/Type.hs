{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Type where

import Data.Display

data Ty
  = TyBool
  | TyNat
  deriving (Eq, Show)

data TypeError
  = TypeError String
  deriving (Eq, Show)

class HasType a where
  typeof :: a -> Either TypeError Ty

instance Display Ty where
  toDisplay ty = case ty of
    TyBool -> "Bool"
    TyNat  -> "Nat"

instance HasType Ty where
  typeof = Right
