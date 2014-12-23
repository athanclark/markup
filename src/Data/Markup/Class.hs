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
class Deploy symbol input markup (m :: * -> *) where
  deploy :: symbol -> input -> m markup

-- | Overload extraction of (co)monad
class Monad m => Markup (m :: * -> *) where
  renderMarkup :: m a -> a


instance Markup InlineMarkupM where
  renderMarkup = runInlineMarkupM

instance Markup HostedMarkupM where
  renderMarkup = runHostedMarkupM

instance Markup LocalMarkupM where
  renderMarkup = runLocalMarkupM

