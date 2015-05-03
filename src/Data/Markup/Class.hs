{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Markup.Class where

import           Data.Markup.Types


-- | Overload assets and their markup library, over some deployment
class Deploy symbol input markup (m :: * -> *) where
  deploy :: symbol -> input -> m markup

-- | Overload extraction of (co)monad
class Monad m => Markup (m :: * -> *) where
  renderMarkup :: m a -> a
  toMarkup :: a -> m a

instance Markup InlineMarkupM where
  renderMarkup = runInlineMarkupM
  toMarkup = InlineMarkupM

instance Markup HostedMarkupM where
  renderMarkup = runHostedMarkupM
  toMarkup = HostedMarkupM

instance Markup LocalMarkupM where
  renderMarkup = runLocalMarkupM
  toMarkup = LocalMarkupM
