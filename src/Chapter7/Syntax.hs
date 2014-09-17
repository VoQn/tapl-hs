{-# LANGUAGE LambdaCase,OverloadedStrings #-}
module Chapter7.Syntax
  ( module Chapter7.Syntax.Term
  , module Chapter7.Syntax.Context
  , module Chapter7.Syntax.Exception
  , module Chapter7.Syntax.BuiltinFuncs
  , indexToName
  , nameToIndex
  , withContext
  ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy.Builder as LB

import Data.Display

import Chapter7.Syntax.Term
import Chapter7.Syntax.Context
import Chapter7.Syntax.Exception
import Chapter7.Syntax.BuiltinFuncs

-------------------------------------------------------------------------------
-- Term Control
-------------------------------------------------------------------------------

indexToName :: Context -> Int -> Int -> Either RuntimeException Name
indexToName c i l
  | length c /= l = Left $ WrongContextDepth i l $ length c
  | length c <= i = Left $ OutOfContextIndex i
  | otherwise     = Right $ c !! i

nameToIndex :: Context -> Name -> Either RuntimeException Int
nameToIndex [] n     = Left $ UnboundIdentifier n
nameToIndex (y:ys) n
  | y == n    = Right 0
  | otherwise = (1 +) <$> nameToIndex ys n

-------------------------------------------------------------------------------
-- Display Term & Exception
-------------------------------------------------------------------------------

withContext :: Context -> Term -> LB.Builder

withContext c (TmAbs n t) = let (c', n') = pickFreshName c n in
  "(" <> spaceSep ["\\", toDisplay n', disp c' t] <> ")"

withContext c (TmApp t1 t2) =
  "(" <> spaceSep (map (disp c) [t1,t2]) <> ")"

withContext c (TmVar i l) =
  eitherDisplay $ indexToName c i l

withContext _ (TmFree n) = toDisplay n

disp :: Context -> Term -> LB.Builder
disp c = toDisplay . withContext c
