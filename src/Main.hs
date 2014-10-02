{-# LANGUAGE LambdaCase #-}
module Main where

import Chapter4 as Ch4
import Chapter7 as Ch7
import Chapter8 as Ch8

import Control.Monad.Trans
import System.Console.Haskeline hiding (display)

data REPL
  = Halt
  | Sarith
  | Sulamb
  | Tyarith
  deriving (Eq, Ord, Show, Enum)

main :: IO ()
main = runInputT defaultSettings repl
  where
  quit = outputStrLn "Goodbye"

  repl = ask >>= \case
    Halt -> quit
    mode -> loop mode

  loop mode = getInputLine (header mode) >>= \case
    Nothing    -> quit
    Just input -> proc mode input >> loop mode

header :: REPL -> String
header = (++ "> ") . show

proc :: (MonadIO m) => REPL -> String -> m ()
proc = \case
  Sarith  -> liftIO . Ch4.process
  Sulamb  -> liftIO . Ch7.process
  Tyarith -> liftIO . Ch8.process
  _ -> undefined

headerLine :: String -> Int -> Char -> String
headerLine l w c =
  let prefix = l ++ " "; rest = w - length prefix
  in  prefix ++ lineGen rest [] c

borderLine :: Int -> Char -> String
borderLine w c = lineGen w [] c

lineGen :: Int -> String -> Char -> String
lineGen 0 s _ = s
lineGen n s c = lineGen (n - 1) (c:s) c

lineWidth :: Int
lineWidth = 40

ask :: InputT IO REPL
ask = do
  mapM_ outputStrLn $
    [ borderLine lineWidth '='
    , " TaPL-hs REPL"
    , borderLine lineWidth '-'
    , " 0) Exit"
    , " 1) Sarith (from Chapter 3-4)"
    , " 2) Sulamb (from Chapter 5-7)"
    , " 3) Tyarith (from Chapter 8)"
    , borderLine lineWidth '='
    ]
  lang <- getInputLine "Select Mode (default: 2) >> "
  let choose = case lang of
        Nothing -> Halt
        Just "" -> Sulamb
        Just  i -> (toEnum (read i))
  mapM_ outputStrLn $
    [ borderLine lineWidth '-'
    , "-- λ" ++ show choose
    , borderLine lineWidth '-'
    ]
  return choose
