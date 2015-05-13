{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , KindSignatures
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module Data.Markup.Class where

import           Data.Markup.Types
import           Control.Comonad

-- | Overload assets and their markup library, over some deployment
class Deploy symbol input markup (m :: * -> *) where
  deploy :: symbol -> input -> m markup

-- | Overload extraction of (co)monad
class Markup (m :: * -> *) where
  renderMarkup :: m a -> a
  toMarkup :: a -> m a

instance (Monad m, Comonad m) => Markup m where
  renderMarkup = extract
  toMarkup = return
