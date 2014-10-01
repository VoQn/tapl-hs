{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SimpleBool.Value where

import Data.Monoid

import Data.Info
import Data.Display (toDisplay)

import SimpleBool.Type
import SimpleBool.Context
import SimpleBool.Syntax

data Val
  = ValTrue               -- ^ true
  | ValFalse              -- ^ false
  | ValAbs Name Type Term -- ^ lambda abstruct
  deriving (Eq, Show)

instance HasType Val where
  typeof ValTrue  = return TyBool
  typeof ValFalse = return TyBool
  typeof (ValAbs _ ty tm) = typeof tm >>= return . TyArr ty

instance Display Val where
  buildText ValTrue  = return "true"
  buildText ValFalse = return "false"
  buildText (ValAbs n ty tm) = do
    body <- buildText tm
    return $ "\\ " <> toDisplay n <> ":" <> toDisplay ty <> "." <> body
