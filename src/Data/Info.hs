{-# LANGUAGE OverloadedStrings #-}
module Data.Info where

import Data.Monoid

import Data.Display

data Info
  = FileImput { name :: String, line :: Int, column :: Int }
  | Unknown
  deriving (Eq, Show)

instance Display Info where
  toDisplay Unknown = "unknown"
  toDisplay (FileImput { name = f, line = l, column = c }) =
    "file: " <> toDisplay f <>
    " (line: " <> toDisplay l <> ", column: " <> toDisplay c <> ")"
