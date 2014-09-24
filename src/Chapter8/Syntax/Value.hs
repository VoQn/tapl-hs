{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax.Value where

import Data.Display
import Chapter8.Syntax.Type
import Chapter8.Syntax.Number

data Val
  = ValTrue
  | ValFalse
  | ValNum   NumVal
  deriving (Eq, Show)

instance Display Val where
  toDisplay value = case value of
    ValTrue  -> "true"
    ValFalse -> "false"
    ValNum v -> toDisplay v

instance HasType Val where
  typeof value = case value of
    ValTrue  -> Right TyBool
    ValFalse -> Right TyBool
    ValNum _ -> Right TyNat
