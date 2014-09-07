{-# LANGUAGE LambdaCase,OverloadedStrings #-}
module Chapter7.Syntax where

import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy.Builder as LB

import Data.Display

type Name = String

data Term
  = TmVar Int Int     -- TmVar { index :: Int, contextLength :: Int }
  | TmAbs Name Term
  | TmApp Term Term
  deriving (Eq, Show)

data Binding
  = NameBind
  deriving (Eq, Show)

type Context = [(Name, Binding)]

data RuntimeError
  = WrongContextLength Int Int Int
  | OutOfContextIndex Int
  | UnboundIdentifier Name
  deriving (Eq, Show)

instance Display RuntimeError where
  toDisplay = \case

    WrongContextLength vi vl cl ->
      let [i,l,s] = map toDisplay [vi,vl,cl] in
      "[BAD_INDEX] Value has wrong index " <>
      "(index: " <> i <> ", length: " <> l <> ")\n" <>
      "[INFO] Context has (length: " <> s <> ")"

    OutOfContextIndex i ->
      "[NOT_FOUND] Not found variable (index: " <> toDisplay i <> ") in this context"

    UnboundIdentifier n ->
      "[UNBOUND_ID] Identifier " <> toDisplay n <> " is unbound"

withContext :: Context -> Term -> LB.Builder
withContext c = \case

  TmAbs n b ->
    let (c', n') = pickFreshName c n in
    parens $ spaceSep $ ["\\", toDisplay n', disp c' b]

  TmApp f x ->
    parens $ spaceSep $ map (disp c) [f, x]

  TmVar i l ->
    eitherDisplay $ indexToName c i l

  where
  disp :: Context -> Term -> LB.Builder
  disp x y = toDisplay $ withContext x y

  eitherDisplay :: (Display a, Display b) => Either a b -> LB.Builder
  eitherDisplay = either toDisplay toDisplay

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName c n
  | isNameBound c n = pickFreshName c (n ++ "'")
  | otherwise       = (((n, NameBind) : c), n)

isNameBound :: Context -> Name -> Bool
isNameBound c n = case c of
  [] -> False
  (y:ys)
    | fst y == n -> True
    | otherwise  -> isNameBound ys n

indexToName :: Context -> Int -> Int -> Either RuntimeError Name
indexToName c i l
  | length c /= l = Left $ WrongContextLength i l $ length c
  | length c <= i = Left $ OutOfContextIndex i
  | otherwise     = Right $ fst $ c !! i

nameToIndex :: Context -> Name -> Either RuntimeError Int
nameToIndex c n = case c of
  [] -> Left $ UnboundIdentifier n
  (y:ys)
    | fst y == n -> Right 0
    | otherwise  -> (1 +) <$> nameToIndex ys n
