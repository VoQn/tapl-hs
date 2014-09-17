{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter7.Eval where

import Control.Applicative
import Data.Map as Map
import Data.Monoid ((<>))

import Data.Display
import Chapter7.Syntax

data Terminate = NoRuleApplies | UndefinedFunction Name
  deriving (Eq, Show)

instance Display Terminate where
  toDisplay = \case
    NoRuleApplies
      -> "[TERMINATE]"

    UndefinedFunction f
      -> "[ERROR] Undefined Function: " <> toDisplay f

isVal :: Term -> Bool
isVal (TmAbs _ _) = True
isVal (TmFree  n) = case Map.lookup n builtinFuncs of
  Just _  -> True
  Nothing -> False
isVal _           = False

eval1 :: Term -> Either Terminate Term
eval1 = \case
  TmApp v1@(TmAbs _ t) v2
    | isVal v2 -> Right $ substTop v2 t
    | otherwise  -> TmApp v1 <$> eval1 v2
  TmApp t1 t2 ->
    TmApp <$> eval1 t1 <*> pure t2
  TmFree n -> case Map.lookup n builtinFuncs of
    Just f  -> Right f
    Nothing -> Left $ UndefinedFunction n
  _ ->
    Left NoRuleApplies

eval :: Term -> Either Terminate Term
eval t = case eval1 t of
  Right t' -> eval t'
  Left  _  -> Right t
