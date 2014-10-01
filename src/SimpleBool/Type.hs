{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Type where

import Data.Display

data Type
  = TyArr  Type Type -- ^ T -> T
  | TyBool           -- ^ Bool
  deriving (Eq, Show)

instance Display Type where
  toDisplay TyBool = "Bool"
  toDisplay (TyArr ty1 ty2) =
    sep "->" $ map toDisplay [ty1, ty2]
