{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Markup.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class


-- | Mirror of the ReaderT monad transformer - used to coerce markup to /inline/ 
-- rendering.
newtype InlineMarkupT i m a = InlineMarkupT { runInlineMarkupT :: i -> m a }
  deriving (Functor)

instance Applicative f => Applicative (InlineMarkupT i f) where
  (<*>) f x = InlineMarkupT $ \i ->
    (<*>) (runInlineMarkupT f i) (runInlineMarkupT x i)

instance Alternative f => Alternative (InlineMarkupT i f) where
  (<|>) m n = InlineMarkupT $ \i ->
    (<|>) (runInlineMarkupT m i) (runInlineMarkupT n i)

instance Monad m => Monad (InlineMarkupT i m) where
  return x = InlineMarkupT $ \_ -> return x
  (>>=) x f = InlineMarkupT $ \i ->
    runInlineMarkupT x i >>= \y -> runInlineMarkupT (f y) i

instance MonadPlus m => MonadPlus (InlineMarkupT i m) where
  mzero = lift mzero
  x `mplus` y = InlineMarkupT $ \i ->
    (runInlineMarkupT x i) `mplus` (runInlineMarkupT y i)

instance MonadTrans (InlineMarkupT i) where
  lift x = InlineMarkupT $ \_ -> x

instance Monad m => MonadReader i (InlineMarkupT i m) where
  ask = InlineMarkupT $ \i -> return i


-- | Mirror of the ReaderT monad transformer - used to coerce markup to /hosted/ 
-- rendering.
newtype HostedMarkupT i m a = HostedMarkupT { runHostedMarkupT :: i -> m a }
  deriving (Functor)

instance Applicative f => Applicative (HostedMarkupT i f) where
  (<*>) f x = HostedMarkupT $ \i ->
    (<*>) (runHostedMarkupT f i) (runHostedMarkupT x i)

instance Alternative f => Alternative (HostedMarkupT i f) where
  (<|>) m n = HostedMarkupT $ \i ->
    (<|>) (runHostedMarkupT m i) (runHostedMarkupT n i)

instance Monad m => Monad (HostedMarkupT i m) where
  return x = HostedMarkupT $ \_ -> return x
  (>>=) x f = HostedMarkupT $ \i ->
    runHostedMarkupT x i >>= \y -> runHostedMarkupT (f y) i

instance MonadPlus m => MonadPlus (HostedMarkupT i m) where
  mzero = lift mzero
  x `mplus` y = HostedMarkupT $ \i ->
    (runHostedMarkupT x i) `mplus` (runHostedMarkupT y i)

instance MonadTrans (HostedMarkupT i) where
  lift x = HostedMarkupT $ \_ -> x

instance Monad m => MonadReader i (HostedMarkupT i m) where
  ask = HostedMarkupT $ \i -> return i


-- | Mirror of the ReaderT monad transformer - used to coerce markup to /local/ 
-- rendering.
newtype LocalMarkupT i m a = LocalMarkupT { runLocalMarkupT :: i -> m a }
  deriving (Functor)

instance Applicative f => Applicative (LocalMarkupT i f) where
  (<*>) f x = LocalMarkupT $ \i ->
    (<*>) (runLocalMarkupT f i) (runLocalMarkupT x i)

instance Alternative f => Alternative (LocalMarkupT i f) where
  (<|>) m n = LocalMarkupT $ \i ->
    (<|>) (runLocalMarkupT m i) (runLocalMarkupT n i)

instance Monad m => Monad (LocalMarkupT i m) where
  return x = LocalMarkupT $ \_ -> return x
  (>>=) x f = LocalMarkupT $ \i ->
    runLocalMarkupT x i >>= \y -> runLocalMarkupT (f y) i

instance MonadPlus m => MonadPlus (LocalMarkupT i m) where
  mzero = lift mzero
  x `mplus` y = LocalMarkupT $ \i ->
    (runLocalMarkupT x i) `mplus` (runLocalMarkupT y i)

instance MonadTrans (LocalMarkupT i) where
  lift x = LocalMarkupT $ \_ -> x

instance Monad m => MonadReader i (LocalMarkupT i m) where
  ask = LocalMarkupT $ \i -> return i


newtype InlineMarkupM i a = InlineMarkupM { runInlineMarkupM :: i -> a }
  deriving (Functor)

instance Applicative (InlineMarkupM i) where
  (<*>) f a = InlineMarkupM $ \i ->
    runInlineMarkupM f i (runInlineMarkupM a i)

instance Monad (InlineMarkupM i) where
  return = InlineMarkupM . const
  x >>= f = InlineMarkupM $ \i ->
    runInlineMarkupM (f $ runInlineMarkupM x i) i

instance MonadReader i (InlineMarkupM i) where
  ask = InlineMarkupM id


newtype HostedMarkupM i a = HostedMarkupM { runHostedMarkupM :: i -> a }
  deriving (Functor)

instance Applicative (HostedMarkupM i) where
  (<*>) f a = HostedMarkupM $ \i ->
    runHostedMarkupM f i (runHostedMarkupM a i)

instance Monad (HostedMarkupM i) where
  return = HostedMarkupM . const
  x >>= f = HostedMarkupM $ \i ->
    runHostedMarkupM (f $ runHostedMarkupM x i) i

instance MonadReader i (HostedMarkupM i) where
  ask = HostedMarkupM id


newtype LocalMarkupM i a = LocalMarkupM { runLocalMarkupM :: i -> a }
  deriving (Functor)

instance Applicative (LocalMarkupM i) where
  (<*>) f a = LocalMarkupM $ \i ->
    runLocalMarkupM f i (runLocalMarkupM a i)

instance Monad (LocalMarkupM i) where
  return = LocalMarkupM . const
  x >>= f = LocalMarkupM $ \i ->
    runLocalMarkupM (f $ runLocalMarkupM x i) i

instance MonadReader i (LocalMarkupM i) where
  ask = LocalMarkupM id
