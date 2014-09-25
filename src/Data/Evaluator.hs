{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Evaluator where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State hiding (state)

type Eval s e a = ReaderT s (ErrorT e (StateT s IO)) a

runEval :: s -> Eval s e a -> IO (Either e a, s)
runEval state ev = runStateT (runErrorT $ runReaderT ev state) state

class Drawable i a where
  draw :: (Error e) => a -> Eval s e i
