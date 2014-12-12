{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Markup.Types where

import Data.Monoid
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

instance Monoid w => Monoid (InlineMarkupM w) where
  x `mappend` y = InlineMarkupM $
    (runInlineMarkupM x) `mappend` (runInlineMarkupM y)

instance Monad w => Monoid (InlineMarkupM (w a)) where
  x `mappend` y = InlineMarkupM $ do
    (runInlineMarkupM x)
    (runInlineMarkupM y)


newtype HostedMarkupM a = HostedMarkupM {runHostedMarkupM :: a}
  deriving (Functor)

instance Applicative HostedMarkupM where
  (<*>) f x = HostedMarkupM $
    runHostedMarkupM f (runHostedMarkupM x)

instance Monad HostedMarkupM where
  return = HostedMarkupM
  x >>= f = f $ runHostedMarkupM x

instance Monoid w => Monoid (HostedMarkupM w) where
  x `mappend` y = HostedMarkupM $
    (runHostedMarkupM x) `mappend` (runHostedMarkupM y)

instance Monad w => Monoid (HostedMarkupM (w a)) where
  x `mappend` y = HostedMarkupM $ do
    (runHostedMarkupM x)
    (runHostedMarkupM y)


newtype LocalMarkupM a = LocalMarkupM {runLocalMarkupM :: a}
  deriving (Functor)

instance Applicative LocalMarkupM where
  (<*>) f x = LocalMarkupM $
    runLocalMarkupM f (runLocalMarkupM x)

instance Monad LocalMarkupM where
  return = LocalMarkupM
  x >>= f = f $ runLocalMarkupM x

instance Monoid w => Monoid (LocalMarkupM w) where
  x `mappend` y = LocalMarkupM $
    (runLocalMarkupM x) `mappend` (runLocalMarkupM y)

instance Monad w => Monoid (LocalMarkupM (w a)) where
  x `mappend` y = LocalMarkupM $ do
    (runLocalMarkupM x)
    (runLocalMarkupM y)
