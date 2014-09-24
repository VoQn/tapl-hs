module Chapter8
  ( module Data.Display
  , module Chapter8.Syntax
  , module Chapter8.Eval
  , process
  ) where

import Data.Display

import Chapter8.Syntax
import Chapter8.Parser
import Chapter8.Eval

process :: String -> IO ()
process line = case parseTerm line of
  Left  err  -> print err
  Right term -> case eval term of
    Left err    -> display err
    Right value -> display value
