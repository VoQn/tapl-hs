{-# LANGUAGE LambdaCase #-}
module Chapter8.Eval where

import Chapter8.Type
import Chapter8.NumVal
import Chapter8.Syntax

eval :: Term -> Either TypeError Val
eval = \case
  TmTrue  -> Right ValTrue
  TmFalse -> Right ValFalse
  TmZero  -> Right $ ValNum NumZero
  _ -> undefined
