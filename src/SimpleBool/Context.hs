{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleBool.Context where

import Control.Monad.Error hiding (Error)
import qualified Control.Monad.Error as ME
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Info
import qualified Data.Evaluator as EV
import SimpleBool.Type

data Binding
  = NameBind      -- ^ λx.x (identifier binding with name)
  | VarBind  Type -- ^ λx:T (identifier binding with type)
  deriving (Eq, Show)

type Name = String

type Context = [(Name, Binding)]

data Env a = Env { symbols :: Map Name a, context :: Context }
  deriving (Eq, Show)

initEnv :: Env a
initEnv = Env { symbols = Map.empty, context = [] }

type Message = String

data Error
  = WrongBinding   Info Name      -- ^ wrong kind of binding for variable
  | OutOfContext   Info Int  Int  -- ^ wrong index of context
  | NotFoundNamed  Info Name      -- ^ not found name binding variable
  | MismatchType   Info Type Type -- ^ mismatch types
  | IsNotArrow     Info Type      -- ^ expected arrow type, but recieved others
  | DifferentType  Info Type Type -- ^ include multiple types in expression
  | SomethingWrong Message
  deriving (Eq, Show)

instance ME.Error Error where
  noMsg  = SomethingWrong "Something wrong"
  strMsg = SomethingWrong

type Eval b a = EV.Eval (Env b) Error a

class HasType a where
  typeof :: a -> Eval b Type

runEval :: Env b -> Eval b a -> IO (Either Error a, Env b)
runEval = EV.runEval

pushContext :: MonadReader (Env a) m => Name -> Binding -> m (Env a)
pushContext x b = do
  env <- ask
  let tabl = symbols env
  let ctx' = (x, b) : (context env)
  return $ Env { symbols = tabl, context = ctx' }

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
    [] -> throwError $ NotFoundNamed info x
    ((y,NameBind):ctx)
      | x == y -> return c
      | otherwise -> search (c + 1) ctx
    (_:ctx) -> search (c + 1) ctx

getTypeFromContext :: Info -> Int -> Eval b Type
getTypeFromContext info i = getBinding info i >>= \case
  VarBind ty -> return ty
  _ -> indexToName info i >>= throwError . WrongBinding info
