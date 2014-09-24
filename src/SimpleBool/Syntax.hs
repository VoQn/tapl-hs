{-# LANGUAGE LambdaCase #-}
module SimpleBool.Syntax where

import Control.Monad.Error hiding (Error)
import Control.Monad.Reader
import Control.Monad.State

data Info
  = FileImput { name :: String, line :: Int, column :: Int }
  | Unknown
  deriving (Eq, Show)

data Type
  = TyArr  Type Type -- ^ T -> T
  | TyBool           -- ^ Bool
  deriving (Eq, Show)

data Binding
  = NameBind      -- ^ λx.x (identifier binding with name)
  | VarBind  Type -- ^ λx:T (identifier binding with type)
  deriving (Eq, Show)

type Name = String

type Context = [(Name, Binding)]

data Term
  = TmVar   Info Int  Int       -- ^ λ.0(1) (bound variable)
  | TmAbs   Info Name Type Term -- ^ λx:T.x (lambda abstruct)
  | TmApp   Info Term Term      -- ^ t t    (apply function)
  | TmTrue  Info                -- ^ true   (boolean value)
  | TmFalse Info                -- ^ false  (boolean value)
  | TmIf    Info Term Term Term -- ^ if <term> <term> <term>
  deriving (Eq, Show)

data Val
  = ValTrue               -- ^ true
  | ValFalse              -- ^ false
  | ValAbs Name Type Term -- ^ lambda abstruct
  deriving (Eq, Show)

data Error
  = WrongBinding  Info Name      -- ^ wrong kind of binding for variable
  | OutOfContext  Info Int  Int  -- ^ wrong index of context
  | NotFoundNamed Info Name      -- ^ not found name binding variable
  | MismatchType  Info Type Type -- ^ mismatch types
  | IsNotArrow    Info Type      -- ^ expected arrow type, but recieved others
  | DifferentType Info Type Type -- ^ include multiple types in expression
  deriving (Eq, Show)

type Eval a = ReaderT Context (ErrorT String (StateT Context IO)) a

class HasType a where
  typeof :: a -> Eval Type

runEval :: Context -> Eval a -> IO (Either String a, Context)
runEval ctx ev = runStateT (runErrorT $ runReaderT ev ctx) ctx

getBindingByIndex :: Info -> Int -> Eval (Name, Binding)
getBindingByIndex info i = do
  ctx <- ask
  let l = length ctx
  if l > i
    then return $ ctx !! i
    else throwError $ show $ OutOfContext info i l

getBinding :: Info -> Int -> Eval Binding
getBinding info i = getBindingByIndex info i >>= return . snd

indexToName :: Info -> Int -> Eval Name
indexToName info i = getBindingByIndex info i >>= return . fst

nameToIndex :: Info -> Name -> Eval Int
nameToIndex info n = ask >>= search n 0
  where
  search name count = \case
    [] -> throwError $ show $ NotFoundNamed info name
    ((x,NameBind):ctx)
      | name == x -> return count
      | otherwise -> search name (count + 1) ctx
    (_:ctx) -> search name (count + 1) ctx

getTypeFromContext :: Info -> Int -> Eval Type
getTypeFromContext info i = getBinding info i >>= \case
  VarBind ty -> return ty
  _ -> indexToName info i >>= throwError . show . WrongBinding info

instance HasType Term where
  typeof (TmTrue  _) = return TyBool
  typeof (TmFalse _) = return TyBool

  typeof (TmVar fi i _) = getTypeFromContext fi i

  typeof (TmAbs _ n ty tm) = do
    ctx <- ask
    let ctx' = (n, VarBind ty) : ctx
    ty' <- local (const ctx') (typeof tm)
    return $ TyArr ty ty'

  typeof (TmApp fi t1 t2) = do
    ty1 <- typeof t1
    ty2 <- typeof t2
    case ty1 of
      TyArr ty1' ty2'
        | ty2 == ty1' -> return ty2
        | otherwise   -> throwError $ show $ MismatchType fi ty1' ty2
      _ -> throwError $ show $ IsNotArrow fi ty1

  typeof (TmIf fi t1 t2 t3) = do
    ty1 <- typeof t1
    case ty1 of
      TyBool -> do
        ty2 <- typeof t2
        ty3 <- typeof t3
        if ty2 == ty3
          then return ty2
          else throwError $ show $ DifferentType fi ty2 ty3
      _ -> throwError $ show $ MismatchType fi TyBool ty1

instance HasType Val where
  typeof ValTrue  = return TyBool
  typeof ValFalse = return TyBool
  typeof (ValAbs _ ty tm) = typeof tm >>= return . TyArr ty
