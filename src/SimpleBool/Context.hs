{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Context where

import Control.Applicative
import Control.Monad
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

runEval :: Env b -> Eval b a -> (Either Error a, Env b)
runEval env ev = Eval.runEval ev env

runEvalTop :: Eval b a -> (Either Error a, Env b)
runEvalTop ev = Eval.runEval ev initEnv

getEvaledResult :: (Either Error a, Env b) -> Either Error a
getEvaledResult = fst

getEvaledContext :: (Either Error a, Env b) -> Context
getEvaledContext = context . snd

class HasType a where
  typeof :: a -> Eval b Type

class HasInfo a where
  inform :: a -> Eval b Info

class Display a where
  buildText :: a -> Eval b LB.Builder

pushBind :: Name -> Binding -> Env b -> Env b
pushBind x b env =
  let ctx' = (x, b) : context env
  in env { context = ctx' }

pushContext :: (MonadReader (Env v) m, Functor m) => Name -> Binding -> m (Env v)
pushContext x b = pushBind x b <$> ask

putContext :: (MonadState (Env v) m) => Name -> Binding -> m ()
putContext x b = put . pushBind x b =<< get

askContext :: (MonadReader (Env v) m) => m Context
askContext = liftM context ask

getBind :: Info -> Int -> Eval b (Name, Binding)
getBind info i = do
  ctx <- askContext
  let l = length ctx
  if l > i
    then return $ ctx !! i
    else throwError $ OutOfContext info i l

getBinding :: Info -> Int -> Eval b Binding
getBinding info i = snd <$> getBind info i

indexToName :: Info -> Int -> Eval b Name
indexToName info i = fst <$> getBind info i

nameToIndex :: Info -> Name -> Eval b Int
nameToIndex info x = askContext >>= search 0
  where
  search _ [] = throwError $ UndefinedSymbol info x
  search c ((y, NameBind) : ctx)
    | x == y    = return c
    | otherwise = search (c + 1) ctx
  search c (_:ctx) = search (c + 1) ctx

getTypeFromContext :: Info -> Int -> Eval b Type
getTypeFromContext info i = getBinding info i >>= \case
  VarBind ty -> return ty
  _ -> indexToName info i >>= throwError . WrongBinding info
