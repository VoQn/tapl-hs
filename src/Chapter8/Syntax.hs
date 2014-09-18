{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Syntax where

import Data.Display

import Chapter8.Type
import Chapter8.NumVal

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

data Val
  = ValTrue
  | ValFalse
  | ValNum NumVal
  deriving (Eq, Show)

instance Display Val where
  toDisplay v = case v of
    ValTrue   -> "true"
    ValFalse  -> "false"
    ValNum nv -> toDisplay nv

instance HasType Val where
  typeof v = case v of
    ValTrue  -> Right TyBool
    ValFalse -> Right TyBool
    ValNum _ -> Right TyNat

instance HasType Term where
  typeof term = case term of
    TmTrue  -> Right TyBool
    TmFalse -> Right TyBool
    TmZero  -> Right TyNat

    TmIsZero t -> case typeof t of
      Right TyNat -> Right TyBool
      Right other -> Left $ MismatchWithRequire TyNat other
      Left  err   -> Left err

    TmSucc t -> case typeof t of
      Right TyNat -> Right TyNat
      Right other -> Left $ MismatchWithRequire TyNat other
      Left  err   -> Left err

    TmPred t -> case typeof t of
      Right TyNat -> Right TyNat
      Right other -> Left $ MismatchWithRequire TyNat other
      Left  err   -> Left err

    TmIf p t f -> case typeof p of
      Right TyBool -> case (typeof t, typeof f) of
        (Right t', Right f')
          | t' == f'  -> Right t'
          | otherwise -> Left $ MultiTypeReturn [("then", t'), ("else", f')]
        (Left err, _) -> Left err
        (_, Left err) -> Left err
      Right ty  -> Left $ MismatchWithRequire TyBool ty
      Left  err -> Left err
