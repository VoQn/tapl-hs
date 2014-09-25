{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Value where

import Data.Info
import Data.Evaluator
import SimpleBool.Type
import SimpleBool.Syntax

data Val
  = ValTrue               -- ^ true
  | ValFalse              -- ^ false
  | ValAbs Name Type Term -- ^ lambda abstruct
  deriving (Eq, Show)

instance Drawable Type Val where
  draw ValTrue = return TyBool
  draw ValFalse = return TyBool
  draw (ValAbs _ ty tm) = typeof tm >>= return . TyArr ty

instance HasType Val where
  typeof = draw
