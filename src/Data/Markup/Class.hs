{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , KindSignatures
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module Data.Markup.Class where

import Control.Comonad


-- | Overload assets and their markup library, over some deployment
class Deploy symbol input markup (m :: * -> *) where
  deploy :: symbol -> input -> m markup

-- | Overloaded monad transformer execution.
class Markup (t :: (* -> *) -> * -> *) where
  renderMarkup :: t m a -> m a
