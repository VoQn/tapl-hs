{-# LANGUAGE OverloadedStrings #-}
module Chapter8.Info where

import Data.Monoid
import Data.Display

data Info
  = FileImput { fileName :: String, line, column :: Int }
  | Unknown
  deriving (Eq, Show)

instance Display Info where
  toDisplay info = case info of
    Unknown -> "unknown"
    FileImput { fileName = f, line = l, column = c } ->
      "file: " <> toDisplay f <>
      " (line: " <> toDisplay l <> ", column: " <> toDisplay c <> ")"
