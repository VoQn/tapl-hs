{-# LANGUAGE OverloadedStrings #-}
module SimpleBool.Error where

import Control.Monad.Error hiding (Error)
import qualified Control.Monad.Error as M
import Data.Monoid

import Data.Info
import Data.Display
import SimpleBool.Type

type Message = String

data Error
  = WrongBinding    Info Name      -- ^ wrong kind of binding for variable
  | OutOfContext    Info Int  Int  -- ^ wrong index of context
  | UndefinedSymbol Info Name      -- ^ undefined symbol binding variable
  | MismatchType    Info Type Type -- ^ mismatch types
  | IsNotArrow      Info Type      -- ^ expected arrow type, but recieved others
  | DifferentType   Info Type Type -- ^ include multiple types in expression
  | SomethingWrong  Message        -- ^ something went wrong (for MonadError only)
  | NoRuleApplies                  -- ^ terminate of evaluate
  deriving (Eq, Show)

instance M.Error Error where
  noMsg  = SomethingWrong "Something wrong"
  strMsg = SomethingWrong

instance Display Error where
  toDisplay err = case err of
    NoRuleApplies ->
      "[TERMINATE] NoRuleApplies"

    SomethingWrong msg ->
      "[ERROR] Something went wrong : " <> toDisplay msg

    WrongBinding fi n ->
      "[ERROR] Wrong kind of binding for variable : " <>
      toDisplay n <> "\n" <> toDisplay fi

    OutOfContext fi i l ->
      "[ERROR] Out of Index of the Context : " <>
      "(index: " <> toDisplay i <> ", context-depth: " <> toDisplay l <> ")\n" <>
      toDisplay fi

    UndefinedSymbol fi n ->
      "[ERROR] Undefined symbol : "　<> toDisplay n <> "\n" <>
      toDisplay fi

    MismatchType fi ty1 ty2 ->
      "[ERROR] Mismatch Type : " <>
      toDisplay ty1 <> " with " <> toDisplay ty2 <> "\n" <>
      toDisplay fi
    _ -> undefined
