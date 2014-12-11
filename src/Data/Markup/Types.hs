{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Markup.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class



newtype InlineMarkupT m a = InlineMarkupT { runInlineMarkupT :: m a }
  deriving (Functor)

instance Applicative f => Applicative (InlineMarkupT f) where
  (<*>) f x = InlineMarkupT $
    (<*>) (runInlineMarkupT f) $ runInlineMarkupT x

instance Monad m => Monad (InlineMarkupT m) where
  return = InlineMarkupT . return
  x >>= f = InlineMarkupT $ 
    runInlineMarkupT x >>= (runInlineMarkupT . f)

instance MonadTrans InlineMarkupT where
  lift = InlineMarkupT

newtype HostedMarkupT m a = HostedMarkupT { runHostedMarkupT :: m a }
  deriving (Functor)

instance Applicative f => Applicative (HostedMarkupT f) where
  (<*>) f x = HostedMarkupT $
    (<*>) (runHostedMarkupT f) $ runHostedMarkupT x

instance Monad m => Monad (HostedMarkupT m) where
  return = HostedMarkupT . return
  x >>= f = HostedMarkupT $ 
    runHostedMarkupT x >>= (runHostedMarkupT . f)

instance MonadTrans HostedMarkupT where
  lift = HostedMarkupT

newtype LocalMarkupT m a = LocalMarkupT { runLocalMarkupT :: m a }
  deriving (Functor)

instance Applicative f => Applicative (LocalMarkupT f) where
  (<*>) f x = LocalMarkupT $
    (<*>) (runLocalMarkupT f) $ runLocalMarkupT x

instance Monad m => Monad (LocalMarkupT m) where
  return = LocalMarkupT . return
  x >>= f = LocalMarkupT $ 
    runLocalMarkupT x >>= (runLocalMarkupT . f)

instance MonadTrans LocalMarkupT where
  lift = LocalMarkupT



newtype InlineMarkupM a = InlineMarkupM {runInlineMarkupM :: a}
  deriving (Functor)

instance Applicative InlineMarkupM where
  (<*>) f x = InlineMarkupM $
    runInlineMarkupM f (runInlineMarkupM x)

instance Monad InlineMarkupM where
  return = InlineMarkupM
  x >>= f = f $ runInlineMarkupM x

newtype HostedMarkupM a = HostedMarkupM {runHostedMarkupM :: a}
  deriving (Functor)

instance Applicative HostedMarkupM where
  (<*>) f x = HostedMarkupM $
    runHostedMarkupM f (runHostedMarkupM x)

instance Monad HostedMarkupM where
  return = HostedMarkupM
  x >>= f = f $ runHostedMarkupM x

newtype LocalMarkupM a = LocalMarkupM {runLocalMarkupM :: a}
  deriving (Functor)

instance Applicative LocalMarkupM where
  (<*>) f x = LocalMarkupM $
    runLocalMarkupM f (runLocalMarkupM x)

instance Monad LocalMarkupM where
  return = LocalMarkupM
  x >>= f = f $ runLocalMarkupM x


