{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax.Type where

import Data.Monoid
import Data.Display

data Ty
  = TyBool
  | TyNat
  deriving (Eq, Show)

data TypeError
  = MismatchWithRequire Ty Ty
  | MultiTypeReturn [(String, Ty)]
  deriving (Eq, Show)

class HasType a where
  typeof :: a -> Either TypeError Ty

instance Display Ty where
  toDisplay ty = case ty of
    TyBool -> "Bool"
    TyNat  -> "Nat"

instance HasType Ty where
  typeof = Right

instance Display TypeError where
  toDisplay err = case err of
    MismatchWithRequire r b
      -> let (r', b') = (toDisplay r, toDisplay b) in
         "Required " <> r' <> " value, but applied value has " <> b'

    MultiTypeReturn cases
      -> "Multiple Type Return [" <> conc cases <> "]"
    where
    conc = sep ", " . map (\(a,b) -> toDisplay a <> ": " <> toDisplay b)
