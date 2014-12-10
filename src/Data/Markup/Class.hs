{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Markup.Class where

import Data.Markup.Types


class Markup markup t (m :: * -> *) where
  renderMarkup :: t -> m markup


class InlineMarkup markup t input where
  renderInline :: t -> input -> markup

class HostedMarkup markup t input where
  renderHosted :: t -> input -> markup

class LocalMarkup markup t input where
  renderLocal :: t -> input -> markup


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
