module Main where

import Chapter4 as Ch4
import Chapter7 as Ch7

import Control.Monad.Trans
import System.Console.Haskeline hiding (display)

data REPL
  = Halt
  | Sarith
  | Sulamb
  deriving (Eq, Ord, Show, Enum)

main :: IO ()
main = runInputT defaultSettings repl
  where
  repl = do
    mode <- ask
    case mode of
      Halt -> outputStrLn "Goodbye"
      _    -> loop mode
  loop mode = do
    expr <- getInputLine $ header mode
    case expr of
      Nothing -> outputStrLn "Goodbye"
      Just input -> proc mode input >> loop mode

header :: REPL -> String
header = (++ "> ") . show

proc :: (MonadIO m) => REPL -> String -> m ()
proc mode = case mode of
  Sarith -> liftIO . Ch4.process
  Sulamb -> liftIO . Ch7.process

ask :: InputT IO REPL
ask = do
  mapM_ outputStrLn [
      "TaPL-hs REPL"
    , "========================================"
    , "0) Exit"
    , "1) Sarith (from Chapter 3-4)"
    , "2) Sulamb (from Chapter 5-7)"
    , "----------------------------------------"
    ]
  lang <- getInputLine "Select Mode (default: 2) >> "
  case lang of
    Nothing -> return Halt
    Just "" -> return Sulamb
    Just  i -> return (toEnum (read i))
