{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Markup.Class where

import Data.Markup.Types

import Lucid
import Data.Monoid
import qualified Data.Text as T

-- | Overload assets and their markup library, over some deployment
class Markup markup t (m :: * -> *) where
  renderMarkup :: t -> m markup

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
