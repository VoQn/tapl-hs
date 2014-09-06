{-# LANGUAGE LambdaCase #-}
module Main where

import Syntax
import Parser
import Eval
import Display

import Control.Monad.Trans
import System.Console.Haskeline hiding (display)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = getInputLine "tapl> " >>= \case
    Nothing -> outputStrLn "Goodbye"
    Just input -> (liftIO $ process input) >> loop

process :: String -> IO ()
process line = case parseExpr line of
  Left  err  -> print err
  Right expr -> case eval expr of
    Left  err  -> display err
    Right term -> display term
