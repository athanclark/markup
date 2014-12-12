{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Markup.Class where

import Data.Markup.Types

import Lucid
import Data.Monoid
import qualified Data.Text as T

import Data.Functor.Identity

-- | Overload assets and their markup library, over some deployment
class Markup m => Deploy symbol input markup (m :: * -> *) where
  deploy :: symbol -> input -> m markup

-- | Overload extraction of monad
class Markup (m :: * -> *) where
  renderMarkup :: m a -> a
  toMarkup :: a -> m a

instance Markup m => Monad m where
  return = toMarkup
  x >>= f = f (renderMarkup x)
{-
instance Markup InlineMarkupM where
  renderMarkup = runInlineMarkupM
  toMarkup = InlineMarkupM

instance Markup HostedMarkupM where
  renderMarkup = runHostedMarkupM
  toMarkup = HostedMarkupM

instance Markup LocalMarkupM where
  renderMarkup = runLocalMarkupM
  toMarkup = LocalMarkupM
-}
