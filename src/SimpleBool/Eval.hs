module SimpleBool.Eval where

import Control.Applicative
import Control.Monad.Error (throwError, catchError)

import SimpleBool.Context
import SimpleBool.Error
import SimpleBool.Syntax

isValue :: Term -> Bool
isValue tm = case tm of
  TmAbs   _ _ _ _ -> True
  TmTrue  _ -> True
  TmFalse _ -> True
  _ -> False

evalTerm1 :: Term -> Eval Term Term
evalTerm1 (TmApp fi t1 t2) = case t1 of
  TmAbs _ _ _ t1'
    | isValue t2 -> return $ substTermTop t2 t1'
    | otherwise  -> throwError NoRuleApplies
  _
    | isValue t1 -> (TmApp fi t1) <$> evalTerm1 t2
    | otherwise  -> (\tm -> TmApp fi tm t2) <$> evalTerm1 t1

evalTerm1 (TmIf fi t1 t2 t3) = case t1 of
  TmTrue  _ -> return t2
  TmFalse _ -> return t3
  _ -> (\tm -> TmIf fi tm t2 t3) <$> evalTerm1 t1

evalTerm1 _ = throwError NoRuleApplies

evalTerm :: Term -> Eval Term Term
evalTerm tm = (evalTerm1 tm >>= evalTerm) `catchError` handler
  where
  handler :: Error -> Eval Term Term
  handler _ = return tm
