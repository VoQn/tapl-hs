{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter7.Exception where

import qualified Data.Text.Lazy.Builder as LB
import Data.Monoid
import Data.Display

import Chapter7.Identifier

data RuntimeException
  = WrongContextDepth Int Int Int
  | OutOfContextIndex Int
  | UnboundIdentifier Name
  deriving (Eq, Show)

instance Display RuntimeException where
  toDisplay = \case

    WrongContextDepth vi vl cl ->
      let [i,l,s] = map toDisplay [vi,vl,cl] in
      "[BAD_INDEX] Value has wrong index " <>
      "(index: " <> i <> ", length: " <> l <> ")\n" <>
      "[INFO] Context has (length: " <> s <> ")"

    OutOfContextIndex i ->
      "[NOT_FOUND] Not found variable " <>
      "(index: " <> toDisplay i <> ") in this context"

    UnboundIdentifier n ->
      "[UNBOUND_ID] Identifier \"" <> toDisplay n <> "\" is unbound"

eitherDisplay :: (Display a, Display b) => Either a b -> LB.Builder
eitherDisplay = either toDisplay toDisplay
