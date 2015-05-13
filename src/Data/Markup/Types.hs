{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module Data.Markup.Types where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Comonad
import           Data.Monoid



newtype InlineMarkupT m a = InlineMarkupT { runInlineMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (InlineMarkupT m a)
deriving instance Applicative f => Applicative (InlineMarkupT f)
deriving instance Monad m => Monad (InlineMarkupT m)

instance (Comonad m, Monad m) => Comonad (InlineMarkupT m) where
  extract = extract . runInlineMarkupT
  duplicate = InlineMarkupT . return

instance MonadTrans InlineMarkupT where
  lift = InlineMarkupT

newtype HostedMarkupT m a = HostedMarkupT { runHostedMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (HostedMarkupT m a)
deriving instance Applicative f => Applicative (HostedMarkupT f)
deriving instance Monad m => Monad (HostedMarkupT m)

instance (Comonad m, Monad m) => Comonad (HostedMarkupT m) where
  extract = extract . runHostedMarkupT
  duplicate = HostedMarkupT . return

instance MonadTrans HostedMarkupT where
  lift = HostedMarkupT

newtype LocalMarkupT m a = LocalMarkupT { runLocalMarkupT :: m a }
  deriving (Functor)

deriving instance Monoid (m a) => Monoid (LocalMarkupT m a)
deriving instance Applicative f => Applicative (LocalMarkupT f)
deriving instance Monad m => Monad (LocalMarkupT m)

instance (Comonad m, Monad m) => Comonad (LocalMarkupT m) where
  extract = extract . runLocalMarkupT
  duplicate = LocalMarkupT . return

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

instance Comonad InlineMarkupM where
  extract = runInlineMarkupM
  duplicate = InlineMarkupM

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

instance Comonad HostedMarkupM where
  extract = runHostedMarkupM
  duplicate = HostedMarkupM

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

instance Comonad LocalMarkupM where
  extract = runLocalMarkupM
  duplicate = LocalMarkupM
