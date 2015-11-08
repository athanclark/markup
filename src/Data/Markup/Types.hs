{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , StandaloneDeriving
  , UndecidableInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module Data.Markup.Types where

import Data.Markup.Class

import Data.Url
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Morph


-- * Inline Deployment

newtype InlineMarkupT m a = InlineMarkupT
  { runInlineMarkupT :: m a
  } deriving (Monoid, Functor, Applicative, Alternative, Monad, MonadIO, MonadFix
             , MonadPlus, MonadReader r, MonadState s, MonadWriter w, MonadRWS r w s
             , MonadError e, MonadThrow, MonadCatch, MonadMask, MonadCont, MonadBase b)

type InlineMarkup = InlineMarkupT Identity

instance ( MonadUrl b m
         , MonadThrow m
         ) => MonadUrl b (InlineMarkupT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl
  symbolUrl = symbolUrl

instance MFunctor InlineMarkupT where
  hoist f (InlineMarkupT x) = InlineMarkupT (f x)

instance MMonad InlineMarkupT where
  embed f (InlineMarkupT x) = f x

instance MonadTransControl InlineMarkupT where
  type StT InlineMarkupT a = a
  liftWith f = InlineMarkupT (f runInlineMarkupT)
  restoreT = InlineMarkupT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (InlineMarkupT m) where
  type StM (InlineMarkupT m) a = ComposeSt InlineMarkupT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance Markup InlineMarkupT where
  renderMarkup = runInlineMarkupT

instance MonadTrans InlineMarkupT where
  lift = InlineMarkupT


-- * Hosted Deployment

newtype HostedMarkupT m a = HostedMarkupT
  { runHostedMarkupT :: m a
  } deriving (Monoid, Functor, Applicative, Alternative, Monad, MonadIO, MonadFix
             , MonadPlus, MonadReader r, MonadState s, MonadWriter w, MonadRWS r w s
             , MonadError e, MonadThrow, MonadCatch, MonadMask, MonadCont, MonadBase b)

type HostedMarkup = HostedMarkupT Identity

instance ( MonadUrl b m
         , MonadThrow m
         ) => MonadUrl b (HostedMarkupT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl
  symbolUrl = symbolUrl

instance MFunctor HostedMarkupT where
  hoist f (HostedMarkupT x) = HostedMarkupT (f x)

instance MMonad HostedMarkupT where
  embed f (HostedMarkupT x) = f x

instance MonadTransControl HostedMarkupT where
  type StT HostedMarkupT a = a
  liftWith f = HostedMarkupT (f runHostedMarkupT)
  restoreT = HostedMarkupT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (HostedMarkupT m) where
  type StM (HostedMarkupT m) a = ComposeSt HostedMarkupT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance Markup HostedMarkupT where
  renderMarkup = runHostedMarkupT

instance MonadTrans HostedMarkupT where
  lift = HostedMarkupT


-- * Local Deployment

newtype LocalMarkupT m a = LocalMarkupT
  { runLocalMarkupT :: m a
  } deriving (Monoid, Functor, Applicative, Alternative, Monad, MonadIO, MonadFix
             , MonadPlus, MonadReader r, MonadState s, MonadWriter w, MonadRWS r w s
             , MonadError e, MonadThrow, MonadCatch, MonadMask, MonadCont, MonadBase b)

type LocalMarkup = LocalMarkupT Identity

instance ( MonadUrl b m
         , MonadThrow m
         ) => MonadUrl b (LocalMarkupT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl
  symbolUrl = symbolUrl

instance MFunctor LocalMarkupT where
  hoist f (LocalMarkupT x) = LocalMarkupT (f x)

instance MMonad LocalMarkupT where
  embed f (LocalMarkupT x) = f x

instance MonadTransControl LocalMarkupT where
  type StT LocalMarkupT a = a
  liftWith f = LocalMarkupT (f runLocalMarkupT)
  restoreT = LocalMarkupT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (LocalMarkupT m) where
  type StM (LocalMarkupT m) a = ComposeSt LocalMarkupT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance Markup LocalMarkupT where
  renderMarkup = runLocalMarkupT

instance MonadTrans LocalMarkupT where
  lift = LocalMarkupT

