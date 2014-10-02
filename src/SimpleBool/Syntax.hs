{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Syntax where

import Data.Monoid
import Control.Monad.Error hiding (Error)
import Control.Monad.Reader

import Data.Info (Info, Name)
import Data.Display (toDisplay, parens, spaceSep)

import SimpleBool.Type
import SimpleBool.Error
import SimpleBool.Context hiding (Eval)

data Term
  = TmVar   Info Int  Int       -- ^ λ.0(1) (bound variable)
  | TmAbs   Info Name Type Term -- ^ λx:T.x (lambda abstruct)
  | TmApp   Info Term Term      -- ^ t t    (apply function)
  | TmTrue  Info                -- ^ true   (boolean value)
  | TmFalse Info                -- ^ false  (boolean value)
  | TmIf    Info Term Term Term -- ^ if <term> <term> <term>
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Shift (Modify Variable's Index & Context Depth)
-------------------------------------------------------------------------------
mapTerm :: (Int -> Info -> Int -> Int -> Term) -> Int -> Term -> Term
mapTerm onvar c0 = walk c0
  where
    walk :: Int -> Term -> Term
    walk c = \case
      TmVar fi i  l     -> onvar c fi i l
      TmAbs fi x  ty t2 -> TmAbs fi x ty $ walk (c + 1) t2
      TmApp fi t1 t2    -> TmApp fi (walk c t1) (walk c t2)
      TmIf  fi t1 t2 t3 -> TmIf  fi (walk c t1) (walk c t2) (walk c t3)
      tm -> tm

shiftTermAbove :: Int -> Int -> Term -> Term
shiftTermAbove d c0 = mapTerm shiftVar c0
  where
  shiftVar c fi i l
    | i >= c    = TmVar fi (i + d) (l + d)
    | otherwise = TmVar fi i (l + d)

shiftTerm :: Int -> Term -> Term
shiftTerm d = shiftTermAbove d 0

substTerm :: Int -> Term -> Term -> Term
substTerm d s t = mapTerm modify d t
  where
  modify c fi i l
    | i == c    = shiftTerm c s
    | otherwise = TmVar fi i l

substTermTop :: Term -> Term -> Term
substTermTop s = shiftTerm (-1) . substTerm 0 (shiftTerm 1 s)

instance HasType Term where
  typeof (TmTrue  _) = return TyBool
  typeof (TmFalse _) = return TyBool

  typeof (TmVar fi i _) = getTypeFromContext fi i

  typeof (TmAbs _ n ty tm) = do
    env' <- pushContext n $ VarBind ty
    ty'  <- local (const env') (typeof tm)
    return $ TyArr ty ty'

  typeof (TmApp fi t1 t2) = do
    ty1 <- typeof t1
    ty2 <- typeof t2
    case ty1 of
      TyArr ty1' ty2'
        | ty2 == ty1' -> return ty2'
        | otherwise   -> throwError $ MismatchType fi ty1' ty2
      _ -> throwError $ IsNotArrow fi ty1

  typeof (TmIf fi t1 t2 t3) = do
    ty1 <- typeof t1
    case ty1 of
      TyBool -> do
        ty2 <- typeof t2
        ty3 <- typeof t3
        if ty2 == ty3
          then return ty2
          else throwError $ CannotTypeUnify fi ty2 ty3
      _ -> throwError $ MismatchType fi TyBool ty1

instance HasInfo Term where
  inform = \case
    TmTrue  info       -> return info
    TmFalse info       -> return info
    TmVar   info _ _   -> return info
    TmApp   info _ _   -> return info
    TmAbs   info _ _ _ -> return info
    TmIf    info _ _ _ -> return info

instance Display Term where
  buildText (TmTrue _)  = return "true"
  buildText (TmFalse _) = return "false"

  buildText (TmVar fi i _) = do
    var <- indexToName fi i
    return $ toDisplay var

  buildText (TmAbs _ n ty tm) = do
    tm' <- buildText tm
    return $ "\\" <> toDisplay n <> ":" <> toDisplay ty <> "." <> tm'

  buildText (TmApp _ t1 t2) = do
    [t1', t2'] <- mapM buildText [t1, t2]
    return $ parens $ spaceSep [t1', t2']

  buildText (TmIf _ t1 t2 t3) = do
    [t1',t2',t3'] <- mapM buildText [t1, t2, t3]
    return $ parens $ spaceSep ["if", t1', "then", t2', "else", t3']
