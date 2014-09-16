module Main where

import Chapter4 as Ch4
import Chapter7 as Ch7

import Control.Monad.Trans
import System.Console.Haskeline hiding (display)

data REPL
  = Sarith
  | Sulamb
  deriving (Eq, Ord, Show, Enum)

main :: IO ()
main = runInputT defaultSettings repl
  where
  repl = do
    mode <- ask
    loop mode
  loop mode = do
    expr <- getInputLine $ header mode
    case expr of
      Nothing -> outputStrLn "Goodbye"
      Just input -> (proc mode input) >> loop mode

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
    , "1) Sarith (from Chapter 4)"
    , "2) Sulamb (from Chapter 7)"
    , "----------------------------------------"
    ]
  lang <- getInputLine "Select Language (default: 1) >> "
  case lang of
    Nothing -> return Sarith
    Just  i -> return (toEnum (read i - 1))
