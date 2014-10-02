{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Evaluator where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

newtype Eval s e a = Eval { runEval :: s -> (Either e a, s) }

mapEval :: (a -> b) -> Eval s e a -> Eval s e b
mapEval f m = Eval $ \s' -> case runEval m s' of
  (Left  e, s) -> (Left e, s)
  (Right x, s) -> (Right $ f x, s)

withEval :: (s' -> s) -> Eval s e a -> Eval s' e a
withEval f m = Eval $ \s -> case runEval m (f s) of
  (Left  e, _) -> (Left  e, s)
  (Right x, _) -> (Right x, s)

evalEval :: Eval s e a -> s -> Either e a
evalEval m = fst . runEval m

execEval :: Eval s e a -> s -> s
execEval m = snd . runEval m

instance (Error e) => Error (Eval s e a) where
  noMsg    = Eval $ \s -> (Left noMsg, s)
  strMsg m = Eval $ \s -> (Left $ strMsg m, s)

instance Functor (Eval s e) where
  fmap = mapEval

instance Applicative (Eval s e) where
  pure  x = Eval $ \s -> (Right x, s)
  f <*> m = Eval $ \s -> case runEval f s of
    (Left  e, s') -> (Left e, s')
    (Right g, s') -> runEval (mapEval g m) s'

instance Monad (Eval s e) where
  return  = pure
  m >>= k = Eval $ \s -> case runEval m s of
    (Left  e, s') -> (Left e, s')
    (Right v, s') -> runEval (k v) s'

instance MonadReader s (Eval s e) where
  ask       = Eval $ \s -> (Right s, s)
  local f m = Eval $ runEval m . execEval m . f

instance MonadState s (Eval s e) where
  get   = ask
  put s = Eval $ const $ runEval (return ()) s

instance MonadError e (Eval s e) where
  throwError e = Eval $ \s -> (Left e, s)
  catchError m h = Eval $ \s' -> case runEval m s' of
    (Left  e, s) -> runEval (h e) s
    (Right x, s) -> (Right x, s)

newtype EvalT s e m a = EvalT { runEvalT :: s -> m (Either e a, s) }

mapEvalT :: (Monad m) => (a -> b) -> EvalT s e m a -> EvalT s e m b
mapEvalT f m = EvalT $ runEvalT m >=> \case
  (Left  e, s) -> return (Left e, s)
  (Right x, s) -> return (Right $ f x, s)

withEvalT :: (Monad m) => (s' -> s) -> EvalT s e m a -> EvalT s' e m a
withEvalT f m = EvalT $ \s -> runEvalT m (f s) >>= \case
  (Left  e, _) -> return (Left  e, s)
  (Right x, _) -> return (Right x, s)

evalEvalT :: (Monad m) => EvalT s e m a -> s -> m (Either e a)
evalEvalT m s = liftM fst $ runEvalT m s

execEvalT :: (Monad m) => EvalT s e m a -> s -> m s
execEvalT m s = liftM snd $ runEvalT m s

instance (Monad m) => Functor (EvalT s e m) where
  fmap = mapEvalT

instance (Monad m) => Applicative (EvalT s e m) where
  pure  x = EvalT $ \s -> return (Right x, s)

  f <*> m = EvalT $ runEvalT f >=> \case
    (Left  e, s) -> return (Left e, s)
    (Right g, s) -> runEvalT (mapEvalT g m) s

instance (Monad m) => Monad (EvalT s e m) where
  return  = pure

  m >>= k = EvalT $ runEvalT m >=> \case
    (Left  e, s) -> return (Left e, s)
    (Right x, s) -> runEvalT (k x) s

  fail msg = EvalT $ const $ fail msg

instance (Monad m) => MonadReader s (EvalT s e m) where
  ask       = EvalT $ \s -> return (Right s, s)
  local f m = EvalT $ runEvalT m . f

instance (Monad m) => MonadState s (EvalT s e m) where
  get   = ask
  put s = EvalT $ const $ runEvalT (return ()) s

instance MonadTrans (EvalT s e) where
  lift m = EvalT $ \s -> m >>= \a -> return (Right a, s)

instance (MonadIO m) => MonadIO (EvalT s e m) where
  liftIO = lift . liftIO

instance (MonadError e m) => MonadError e (EvalT s e m) where
  throwError e   = EvalT $ \s -> return (Left e, s)
  catchError m h = EvalT $ runEvalT m >=> \case
    (Left  e, s) -> runEvalT (h e) s
    (Right v, s) -> return (Right v, s)
