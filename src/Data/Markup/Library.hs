{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Markup.Library where

import Data.Markup.Class

import UrlPath

import qualified Lucid as L
import qualified Lucid.Base as LBase

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as HI

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Monoid
import Control.Monad.Trans

-- | Abstract data type of an "image" asset.
data Image = Image deriving (Show, Eq)

-- | Abstract data type of javascript.
data JavaScript = JavaScript deriving (Show, Eq)


-- Lucid instances

-- Images only have local and hosted instances - inline can only be done with 
-- js... TODO?
instance Monad m =>
           HostedMarkup (LBase.HtmlT m ()) Image T.Text where
  renderHosted Image t =
    L.img_ [L.src_ t]

instance ( Monad m
         , Url input m ) =>
             LocalMarkup (LBase.HtmlT m ()) Image input where
  renderLocal Image t = do
    url <- lift $ renderUrl t
    L.img_ [L.src_ url]


-- JS instances
instance Monad m =>
           InlineMarkup (LBase.HtmlT m ()) JavaScript T.Text where
  renderInline JavaScript t =
    L.script_ [] t

instance Monad m =>
           InlineMarkup (LBase.HtmlT m ()) JavaScript LT.Text where
  renderInline JavaScript t =
    L.script_ [] t

instance Monad m =>
           HostedMarkup (LBase.HtmlT m ()) JavaScript T.Text where
  renderHosted JavaScript t =
    L.script_ [L.src_ t] ("" :: T.Text)

instance ( Monad m
         , Url input m ) =>
             LocalMarkup (LBase.HtmlT m ()) JavaScript input where
  renderLocal JavaScript t = do
    url <- lift $ renderUrl t
    L.script_ [L.src_ url] ("" :: T.Text)


-- Blaze-html instances

instance H.ToValue a =>
             HostedMarkup (HI.MarkupM ()) Image a where
  renderHosted Image t =
    H.img H.! A.src (H.toValue t)

instance Url input HI.MarkupM =>
             LocalMarkup (HI.MarkupM ()) Image input where
  renderLocal Image t = do
    url <- renderUrl t
    H.img H.! A.src (H.toValue url)


instance H.ToMarkup a =>
             InlineMarkup (HI.MarkupM ()) JavaScript a where
  renderInline JavaScript t =
    H.script (H.toMarkup t)

instance H.ToValue a =>
             HostedMarkup (HI.MarkupM ()) JavaScript a where
  renderHosted JavaScript t =
    (H.script H.! A.src (H.toValue t)) HI.Empty

instance Url input HI.MarkupM =>
             LocalMarkup (HI.MarkupM ()) JavaScript input where
  renderLocal JavaScript t = do
    url <- renderUrl t
    (H.script H.! A.src (H.toValue url)) HI.Empty
