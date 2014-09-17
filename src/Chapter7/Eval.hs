{-# LANGUAGE OverloadedStrings #-}
module Chapter7.Eval where

import Control.Applicative
import Data.Map as Map
import Data.Monoid ((<>))

import Data.Display

import Chapter7.Identifier
import Chapter7.Syntax
import Chapter7.BuiltinFuncs

data Terminate
  = NoRuleApplies
  | UndefinedFunction Name
  deriving (Eq, Show)

instance Display Terminate where
  toDisplay t = case t of
    NoRuleApplies
      -> "[TERMINATE]"
    UndefinedFunction n
      -> "[ERROR] Undefined Function: " <> toDisplay n

isVal :: Term -> Bool
isVal t = case t of
  TmAbs _ _
    -> True
  TmFree n
    -> case Map.lookup n builtinFuncs of
        Just _  -> True
        Nothing -> False
  _ -> False

eval1 :: Term -> Either Terminate Term
eval1 t = case t of
  TmApp (TmAbs _ t') t2
    -> Right $ substTop t2 t'
  TmApp t1 t2
    -> TmApp <$> eval1 t1 <*> pure t2
  TmFree n
    -> case Map.lookup n builtinFuncs of
        Just fn -> Right fn
        Nothing -> Left $ UndefinedFunction n
  _ -> Left NoRuleApplies

eval :: Term -> Either Terminate Term
eval t = case eval1 t of
  Left t'  -> case t' of
    NoRuleApplies -> Right t
    _             -> Left  t'
  Right t' -> eval t'
