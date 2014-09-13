{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Chapter7.Syntax.Exception where

import Data.Monoid
import Data.Display
import Chapter7.Syntax.Term

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
