{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Context where

import Control.Monad.Error (throwError)
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text.Lazy.Builder as LB

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Info
import qualified Data.Evaluator as Eval

import SimpleBool.Type
import SimpleBool.Error

data Binding
  = NameBind      -- ^ λx.x (identifier binding with name)
  | VarBind  Type -- ^ λx:T (identifier binding with type)
  deriving (Eq, Show)

type Context = [(Name, Binding)]

data Env a = Env { symbols :: Map Name a, context :: Context }
  deriving (Eq, Show)

initEnv :: Env a
initEnv = Env { symbols = Map.empty, context = [] }

type Eval b a = Eval.Eval (Env b) Error a

runEval :: Eval b a -> (Either Error a, Env b)
runEval ev = Eval.runEval ev initEnv

getEvaledContext :: (Either Error a, Env b) -> Context
getEvaledContext = context . snd

class HasType a where
  typeof :: a -> Eval b Type

class HasInfo a where
  inform :: a -> Eval b Info

class Display a where
  buildText :: a -> Eval b LB.Builder

pushContext :: MonadReader (Env v) m => Name -> Binding -> m (Env v)
pushContext x b = do
  env <- ask
  let tabl = symbols env
  let ctx' = (x, b) : (context env)
  return $ Env { symbols = tabl, context = ctx' }

putContext :: MonadState (Env v) m => Name -> Binding -> m ()
putContext x b = do
  env <- get
  let tabl = symbols env
  let ctx' = (x, b) : (context env)
  put $ Env { symbols = tabl, context = ctx' }

getBind :: Info -> Int -> Eval b (Name, Binding)
getBind info i = do
  env <- ask
  let ctx = context env
  let l = length ctx
  if l > i
    then return $ ctx !! i
    else throwError $ OutOfContext info i l

getBinding :: Info -> Int -> Eval b Binding
getBinding info i = getBind info i >>= return . snd

indexToName :: Info -> Int -> Eval b Name
indexToName info i = getBind info i >>= return . fst

nameToIndex :: Info -> Name -> Eval b Int
nameToIndex info x = do
  env <- ask
  let ctx = context env
  search 0 ctx
  where
  search c = \case
    [] -> throwError $ UndefinedSymbol info x
    ((y,NameBind):ctx)
      | x == y -> return c
      | otherwise -> search (c + 1) ctx
    (_:ctx) -> search (c + 1) ctx

getTypeFromContext :: Info -> Int -> Eval b Type
getTypeFromContext info i = getBinding info i >>= \case
  VarBind ty -> return ty
  _ -> indexToName info i >>= throwError . WrongBinding info
