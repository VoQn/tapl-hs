{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Chapter8.Eval (eval) where

import Control.Applicative
import Chapter8.Syntax

eval :: Term -> Either TypeError Val
eval term = typeof term >> eval' term

eval' :: Term -> Either TypeError Val
eval' = \case
  TmTrue  _ -> return $ ValTrue
  TmFalse _ -> return $ ValFalse
  TmZero  _ -> return $ ValNum NumZero

  TmSucc   _ t -> vSucc <$> eval' t
  TmPred   _ t -> vPred <$> eval' t
  TmIsZero _ t -> vIsZero <$> eval' t

  TmIf _ p t f -> eval' p >>= \case
    ValTrue  -> eval' t
    ValFalse -> eval' f

vSucc :: Val -> Val
vSucc (ValNum nv) = ValNum $ NumSucc nv

vPred :: Val -> Val
vPred (ValNum (NumSucc nv)) = ValNum nv
vPred v = v

vIsZero :: Val -> Val
vIsZero (ValNum NumZero) = ValTrue
vIsZero _ = ValFalse
