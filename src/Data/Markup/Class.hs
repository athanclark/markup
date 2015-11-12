{-# LANGUAGE
    MultiParamTypeClasses
  #-}

-- |
-- Module      :  Data.Markup.Class
-- Copyright   :  (c) Athan L. Clark
-- License     :  MIT
--
-- Maintainer  :  Athan L. Clark <athan.clark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- We expect the markup engines that we support to be /monadic/:
--
--   * they accumulate their data internally, in the monad - not a direct value
--   * they do not care about the data contained - <https://hackage.haskell.org/package/lucid lucid>
--     and <https://hackage.haskell.org/package/blaze-html blaze-html> both set the
--     contained data to unit @()@ in their combinators.
--
-- From this, we can make multiple calls to @deploy@ in a @do@ statement, and
-- none of the types will be ambiguous.


module Data.Markup.Class where


-- | Overload assets and their markup library, over some deployment
class Deploy symbol strategy input markup where
  deploy :: symbol -> strategy -> input -> markup ()
