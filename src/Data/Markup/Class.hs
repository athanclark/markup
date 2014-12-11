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
class Markup markup t (m :: * -> *) where
  renderMarkup :: t -> m markup


-- I split the following classes because the same symbol and markup types may 
-- have different input schemas per deployment scheme.
--
-- | Assets that implement this (with their representing markup library) can be 
-- rendered as Inline
class InlineMarkup markup t input where
  renderInline :: t -> input -> markup

-- | Assets that implement this can be rendered as /hosted/.
class HostedMarkup markup t input where
  renderHosted :: t -> input -> markup

-- | Assets that implement this can be rendered as /local/.
class LocalMarkup markup t input where
  renderLocal :: t -> input -> markup

class RenderMarkup m i where
  runMarkupT :: m a -> i -> a

instance ( InlineMarkup markup t input
         , Markup markup t (InlineMarkupM input) ) =>
             RenderMarkup (InlineMarkupM input) input where
  runMarkupT = runInlineMarkupM

instance ( HostedMarkup markup t input
         , Markup markup t (HostedMarkupM input) ) =>
             RenderMarkup (HostedMarkupM input) input where
  runMarkupT = runHostedMarkupM

instance ( LocalMarkup markup t input
         , Markup markup t (LocalMarkupM input) ) =>
             RenderMarkup (LocalMarkupM i) i where
  runMarkupT = runLocalMarkupM

instance ( InlineMarkup markup symbol input
         , Markup markup symbol (InlineMarkupT input Identity) ) =>
             RenderMarkup (InlineMarkupT input Identity) input where
  runMarkupT x i = runIdentity $ runInlineMarkupT x i

instance ( HostedMarkup markup symbol input
         , Markup markup symbol (HostedMarkupT input Identity) ) =>
             RenderMarkup (HostedMarkupT input Identity) input where
  runMarkupT x i = runIdentity $ runHostedMarkupT x i

instance ( LocalMarkup markup symbol input
         , Markup markup symbol (LocalMarkupT input Identity) ) =>
             RenderMarkup (LocalMarkupT input Identity) input where
  runMarkupT x i = runIdentity $ runLocalMarkupT x i

-- TODO: wrap all three deployment systems in one class, and remove the need to 
-- worry about it post-`renderMarkup`.

instance ( InlineMarkup markup t input
         , Monad m ) =>
             Markup markup t (InlineMarkupT input m) where
  renderMarkup t = InlineMarkupT $ \i ->
    return $ renderInline t i

instance ( HostedMarkup markup t input
         , Monad m ) =>
             Markup markup t (HostedMarkupT input m) where
  renderMarkup t = HostedMarkupT $ \i ->
    return $ renderHosted t i

instance ( LocalMarkup markup t input
         , Monad m ) =>
             Markup markup t (LocalMarkupT input m) where
  renderMarkup t = LocalMarkupT $ \i ->
    return $ renderLocal t i


instance ( InlineMarkup markup symbol input ) =>
             Markup markup symbol (InlineMarkupM input) where
  renderMarkup sym = InlineMarkupM $ \i ->
    renderInline sym i

instance ( HostedMarkup markup symbol input ) =>
             Markup markup symbol (HostedMarkupM input) where
  renderMarkup sym = HostedMarkupM $ \i ->
    renderHosted sym i

instance ( LocalMarkup markup symbol input ) =>
             Markup markup symbol (LocalMarkupM input) where
  renderMarkup sym = LocalMarkupM $ \i ->
    renderLocal sym i
