{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Eval where

import Control.Applicative
import Data.Monoid

import Syntax
import Display

type FuncForm = String

data Terminate = NoRuleApplies | TypeError Ty FuncForm Term
  deriving (Eq, Ord, Show)

instance Display Terminate where
  toDisplay = \case
    NoRuleApplies -> "[TERMINATE]"
    TypeError ty form tm -> spaceSep $
      ["[ERROR]", "TypeError", "of", toDisplay form
      , "\nRequire", "(x:" <> toDisplay ty, "Type)"
      , "\nBut", toDisplay tm]

typeError :: Ty -> FuncForm -> Term -> Either Terminate Term
typeError ty form tm = Left $ TypeError ty form tm

eval1 :: Term -> Either Terminate Term
eval1 = \case
  TmIf p t f -> case p of
    TmTrue  -> return t
    TmFalse -> return f
    _
      | isBool p  -> (\p' -> TmIf p' t f) <$> eval1 p
      | otherwise -> typeError TyBool "(if (x) <term> <term>)" p

  TmSucc n
    | isNumeric n -> TmSucc <$> eval1 n
    | otherwise   -> typeError TyNat "(succ x)" n

  TmPred t -> case t of
    TmZero -> return TmZero
    TmSucc n
      | isNumeric n -> return n
      | otherwise   -> typeError TyNat "(succ x)" n
    _
      | isNumeric t -> TmPred <$> eval1 t
      | otherwise   -> typeError TyNat "(pred x)" t

  TmIsZero t -> case t of
    TmZero -> return TmTrue
    TmSucc n
      | isNumeric n -> return TmFalse
      | otherwise   -> typeError TyNat "(succ x)" n
    _
      | isNumeric t -> TmIsZero <$> eval1 t
      | otherwise   -> typeError TyNat "(zero? x)" t

  _ -> Left NoRuleApplies

eval :: Term -> Either Terminate Term
eval t = case eval1 t of
  Left e -> case e of
    NoRuleApplies   -> return t
    TypeError _ _ _ -> Left e

  Right t' -> eval t'
