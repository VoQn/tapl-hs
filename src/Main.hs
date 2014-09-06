{-# LANGUAGE LambdaCase #-}
module Main where

import Chapter4 as Ch4

import Control.Monad.Trans
import System.Console.Haskeline hiding (display)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = getInputLine "tapl> " >>= \case
    Nothing -> outputStrLn "Goodbye"
    Just input -> (liftIO $ Ch4.process input) >> loop
