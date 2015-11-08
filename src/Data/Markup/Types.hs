{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module Data.Markup.Types where

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Comonad


-- * Inline Deployment

newtype InlineMarkupT m a = InlineMarkupT
  { runInlineMarkupT :: m a
  } deriving (Functor)

type InlineMarkup = InlineMarkupT Identity

deriving instance Monoid (m a)  => Monoid      (InlineMarkupT m a)
deriving instance Applicative f => Applicative (InlineMarkupT f)
deriving instance Monad m       => Monad       (InlineMarkupT m)

instance ( Comonad m
         , Monad m
         ) => Comonad (InlineMarkupT m) where
  extract = extract . runInlineMarkupT
  duplicate = InlineMarkupT . return

instance MonadTrans InlineMarkupT where
  lift = InlineMarkupT


-- * Hosted Deployment

newtype HostedMarkupT m a = HostedMarkupT
  { runHostedMarkupT :: m a
  } deriving (Functor)

type HostedMarkup = HostedMarkupT Identity

deriving instance Monoid (m a)  => Monoid      (HostedMarkupT m a)
deriving instance Applicative f => Applicative (HostedMarkupT f)
deriving instance Monad m       => Monad       (HostedMarkupT m)

instance ( Comonad m
         , Monad m
         ) => Comonad (HostedMarkupT m) where
  extract = extract . runHostedMarkupT
  duplicate = HostedMarkupT . return

instance MonadTrans HostedMarkupT where
  lift = HostedMarkupT


-- * Local Deployment

newtype LocalMarkupT m a = LocalMarkupT
  { runLocalMarkupT :: m a
  } deriving (Functor)

type LocalMarkup = LocalMarkupT Identity

deriving instance Monoid (m a)  => Monoid      (LocalMarkupT m a)
deriving instance Applicative f => Applicative (LocalMarkupT f)
deriving instance Monad m       => Monad       (LocalMarkupT m)

instance ( Comonad m
         , Monad m
         ) => Comonad (LocalMarkupT m) where
  extract = extract . runLocalMarkupT
  duplicate = LocalMarkupT . return

instance MonadTrans LocalMarkupT where
  lift = LocalMarkupT

