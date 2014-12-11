{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Markup where
--    ( module Data.Markup.Types
--    , module Data.Markup.Class
--    , module Data.Markup.Library ) where

import Lucid
import Lucid.Base
import UrlPath

import Data.Markup.Types
import Data.Markup.Class
import Data.Markup.Library

import Data.Monoid
import Control.Applicative
import Control.Monad.Trans

-- | At the same time, we want to be able to make an instance for some Asset 
-- in some Markup with some Deployment setting - and thus, the Input required.
class Monad m => Deploy symbol input markup m where
  -- ^ A context must decide what @input@ will be - it has to encode it with the 
  -- combination of deploay, symbol, and markup together.
  deploy :: symbol -> input -> m markup


newtype Inline a = Inline {runInline :: a}
  deriving (Show, Eq, Functor)

instance Applicative Inline where
  (<*>) f x = Inline $
    runInline f $ runInline x

instance Monad Inline where
  return = Inline
  x >>= f = f $ runInline x
