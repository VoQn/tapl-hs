{-# LANGUAGE LambdaCase #-}
module Chapter7.Eval where

import Control.Applicative
import Chapter7.Syntax

data Terminate = NoRuleApplies -- | TypeError Ty FuncForm Term
  deriving (Eq, Show)

instance Display Terminate where
  toDisplay = \case
    NoRuleApplies -> "[TERMINATE]"
--    TypeError ty form tm -> spaceSep $
--      ["[ERROR]", "TypeError", "of", toDisplay form
--      , "\nRequire", "(x:" <> toDisplay ty, "Type)"
--      , "\nBut", toDisplay tm]

isVal :: Context -> Term -> Bool
isVal c = \case
  TmAbs _ _ -> True
  _ -> False

eval1 :: Context -> Term -> Either Terminate Term
eval1 c = \case
  TmApp v1@(TmAbs x t) v2
    | isVal c v2 -> substTop v2 t
    | otherwise  -> TmApp v1 <$> eval1 c v2

  TmApp t1 t2 -> (\ t -> TmApp t t2) <$> eval1 c t1

  _ -> Left NoRuleApplies

eval :: Context -> Term -> Either Terminate Term
eval c t = case eval1 c t of
  Right t' -> eval c t'
  Left  e  -> t
