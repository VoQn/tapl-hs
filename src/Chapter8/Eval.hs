{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Chapter8.Eval (eval) where

import Control.Applicative
import Chapter8.Type
import Chapter8.NumVal
import Chapter8.Syntax

eval :: Term -> Either TypeError Val
eval term = typeof term >> eval' term

eval' :: Term -> Either TypeError Val
eval' = \case
  TmTrue  -> return $ ValTrue
  TmFalse -> return $ ValFalse
  TmZero  -> return $ ValNum NumZero

  TmSucc   t -> vSucc <$> eval' t
  TmPred   t -> vPred <$> eval' t
  TmIsZero t -> vIsZero <$> eval' t

  TmIf p t f -> eval' p >>= \case
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
