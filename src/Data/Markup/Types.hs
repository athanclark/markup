{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Markup.Types where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class



newtype InlineMarkupT m a = InlineMarkupT { runInlineMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (InlineMarkupT m a)
deriving instance Applicative f => Applicative (InlineMarkupT f)
deriving instance Monad m => Monad (InlineMarkupT m)

instance MonadTrans InlineMarkupT where
  lift = InlineMarkupT

newtype HostedMarkupT m a = HostedMarkupT { runHostedMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (HostedMarkupT m a)
deriving instance Applicative f => Applicative (HostedMarkupT f)
deriving instance Monad m => Monad (HostedMarkupT m)

instance MonadTrans HostedMarkupT where
  lift = HostedMarkupT

newtype LocalMarkupT m a = LocalMarkupT { runLocalMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (LocalMarkupT m a)
deriving instance Applicative f => Applicative (LocalMarkupT f)
deriving instance Monad m => Monad (LocalMarkupT m)

instance MonadTrans LocalMarkupT where
  lift = LocalMarkupT



newtype InlineMarkupM a = InlineMarkupM {runInlineMarkupM :: a}
  deriving (Functor)

deriving instance Monoid a => Monoid (InlineMarkupM a)

instance Applicative InlineMarkupM where
  (<*>) f x = InlineMarkupM $
    runInlineMarkupM f (runInlineMarkupM x)
  pure = InlineMarkupM

instance Monad InlineMarkupM where
  return = InlineMarkupM
  x >>= f = f $ runInlineMarkupM x

newtype HostedMarkupM a = HostedMarkupM {runHostedMarkupM :: a}
  deriving (Functor)

deriving instance Monoid a => Monoid (HostedMarkupM a)

instance Applicative HostedMarkupM where
  (<*>) f x = HostedMarkupM $
    runHostedMarkupM f (runHostedMarkupM x)
  pure = HostedMarkupM

instance Monad HostedMarkupM where
  return = HostedMarkupM
  x >>= f = f $ runHostedMarkupM x

newtype LocalMarkupM a = LocalMarkupM {runLocalMarkupM :: a}
  deriving (Functor)

deriving instance Monoid a => Monoid (LocalMarkupM a)

instance Applicative LocalMarkupM where
  (<*>) f x = LocalMarkupM $
    runLocalMarkupM f (runLocalMarkupM x)
  pure = LocalMarkupM

instance Monad LocalMarkupM where
  return = LocalMarkupM
  x >>= f = f $ runLocalMarkupM x
