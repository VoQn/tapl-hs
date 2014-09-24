module Chapter8.Syntax.Number where

import Data.Monoid
import Data.Display

import Chapter8.Syntax.Type

data NumVal
  = NumZero
  | NumSucc NumVal
  deriving (Eq, Show)

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

instance HasType NumVal where
  typeof _ = Right TyNat
