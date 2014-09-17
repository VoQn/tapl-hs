module Chapter7
  ( module Data.Display
  , module Chapter7.Syntax
  , module Chapter7.Parser
  , module Chapter7.Eval
  , process
  ) where

import Data.Display
import Chapter7.Syntax
import Chapter7.Parser (parseExpr)
import Chapter7.Eval

process :: String -> IO ()
process line = case parseExpr line of
  Left  err  -> print err
  Right expr -> case eval expr of
    Left  err  -> display err
    Right term -> display (withContext [] term)
