{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Markup.Library where

import Data.Markup.Class
import Data.Markup.Types

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
           Deploy Image T.Text (LBase.HtmlT m ()) HostedMarkupM where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance Monad m =>
           Deploy Image T.Text (LBase.HtmlT m ()) LocalMarkupM where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance Url input m =>
           Deploy Image input (LBase.HtmlT m ()) LocalMarkupM where
  deploy Image i = return $ do
    url <- lift $ renderUrl i
    L.img_ [L.src_ url]

instance ( Monad m
         , Monad m' ) =>
             Deploy Image T.Text (LBase.HtmlT m ()) (HostedMarkupT m') where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance ( Monad m
         , Monad m' ) =>
             Deploy Image T.Text (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance ( Url input m
         , Monad m' ) =>
             Deploy Image input (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy Image i = return $ do
    url <- lift $ renderUrl i
    L.img_ [L.src_ url]


-- JS instances

instance Monad m =>
           Deploy JavaScript T.Text (LBase.HtmlT m ()) InlineMarkupM where
  deploy JavaScript i = return $
    L.script_ [] i

instance Monad m =>
           Deploy JavaScript LT.Text (LBase.HtmlT m ()) InlineMarkupM where
  deploy JavaScript i = return $
    L.script_ [] i

instance Monad m =>
           Deploy JavaScript T.Text (LBase.HtmlT m ()) HostedMarkupM where
  deploy JavaScript i = return $
    L.script_ [L.src_ i] ("" :: T.Text)

instance Url input m =>
           Deploy JavaScript input (LBase.HtmlT m ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    url <- lift $ renderUrl i
    L.script_ [L.src_ url] ("" :: T.Text)

instance ( Monad m
         , Monad m' ) =>
             Deploy JavaScript T.Text (LBase.HtmlT m ()) (InlineMarkupT m') where
  deploy JavaScript i = return $
    L.script_ [] i

instance ( Monad m
         , Monad m' ) =>
             Deploy JavaScript LT.Text (LBase.HtmlT m ()) (InlineMarkupT m') where
  deploy JavaScript i = return $
    L.script_ [] i

instance ( Monad m
         , Monad m' ) =>
             Deploy JavaScript T.Text (LBase.HtmlT m ()) (HostedMarkupT m') where
  deploy JavaScript i = return $
    L.script_ [L.src_ i] ("" :: T.Text)

instance ( Url input m
         , Monad m' ) =>
             Deploy JavaScript input (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy JavaScript i = return $ do
    url <- lift $ renderUrl i
    L.script_ [L.src_ url] ("" :: T.Text)


-- Blaze-html instances


instance H.ToValue input =>
           Deploy Image input (HI.MarkupM ()) HostedMarkupM where
  deploy Image i = return $
    H.img H.! A.src (H.toValue i)

instance Url input HI.MarkupM =>
           Deploy Image input (HI.MarkupM ()) LocalMarkupM where
  deploy Image i = return $ do
    url <- renderUrl i
    H.img H.! A.src (H.toValue url)

instance ( H.ToValue input
         , Monad m ) =>
             Deploy Image input (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image i = return $
    H.img H.! A.src (H.toValue i)

instance ( Url input HI.MarkupM
         , Monad m ) =>
             Deploy Image input (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = return $ do
    url <- renderUrl i
    H.img H.! A.src (H.toValue url)



instance H.ToMarkup input =>
           Deploy JavaScript input (HI.MarkupM ()) InlineMarkupM where
  deploy JavaScript i = return $
    H.script (H.toMarkup i)

instance H.ToValue input =>
           Deploy JavaScript input (HI.MarkupM ()) HostedMarkupM where
  deploy JavaScript i = return $
    (H.script H.! A.src (H.toValue i)) HI.Empty

instance Url input HI.MarkupM =>
           Deploy JavaScript input (HI.MarkupM ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    url <- renderUrl i
    (H.script H.! A.src (H.toValue url)) HI.Empty

instance ( H.ToMarkup input
         , Monad m ) =>
             Deploy JavaScript input (HI.MarkupM ()) (InlineMarkupT m) where
  deploy JavaScript i = return $
    H.script (H.toMarkup i)

instance ( H.ToValue input
         , Monad m ) =>
             Deploy JavaScript input (HI.MarkupM ()) (HostedMarkupT m) where
  deploy JavaScript i = return $
    (H.script H.! A.src (H.toValue i)) HI.Empty

instance ( Url input HI.MarkupM
         , Monad m ) =>
             Deploy JavaScript input (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = return $ do
    url <- renderUrl i
    (H.script H.! A.src (H.toValue url)) HI.Empty
