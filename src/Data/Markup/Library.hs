{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MonoLocalBinds
  , OverloadedStrings
  , UndecidableInstances
  , ExtendedDefaultRules
  , MultiParamTypeClasses
  #-}

-- |
-- Module      :  Data.Markup.Library
-- Copyright   :  (c) Athan L. Clark
-- License     :  MIT
--
-- Maintainer  :  Athan L. Clark <athan.clark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- This module enumerates the expected behavior for each type of asset to be
-- deployed.

module Data.Markup.Library where

import           Data.Markup.Class (Deploy (deploy))
import           Data.Markup.Types (Inline (..), Remote (..), Locally (..))

import           Data.Url (MonadUrl (locUrl, absDirUrl, absFileUrl), GroundedUrlT, AbsoluteUrlT)
import           Data.URI (printURI)
import           Path (Path, Abs, Dir, File)
import           Path.Extended (Location)

import qualified Lucid                       as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as HI
import qualified Clay                        as C

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import           Control.Monad.Trans (MonadTrans (lift))


data Image        = Image        deriving (Show, Eq)
data JavaScript   = JavaScript   deriving (Show, Eq)
data Css          = Css          deriving (Show, Eq)
data WebComponent = WebComponent deriving (Show, Eq)


-- Images

-- Remote

linkedImageLucid :: Monad m => T.Text -> L.HtmlT m ()
linkedImageLucid link =
  L.img_ [L.src_ link]

instance ( Monad m
         ) => Deploy Image Remote T.Text (L.HtmlT m) where
  deploy Image Remote = linkedImageLucid

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Image Remote (Path Abs File) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Image Remote i = do
    link <- lift (absFileUrl i)
    linkedImageLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Image Remote (Path Abs Dir) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Image Remote i = do
    link <- lift (absDirUrl i)
    linkedImageLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Image Remote Location (L.HtmlT (AbsoluteUrlT m)) where
  deploy Image Remote i = do
    link <- lift (locUrl i)
    linkedImageLucid (printURI link)


-- Local

instance ( Monad m
         ) => Deploy Image Locally T.Text (L.HtmlT m) where
  deploy Image Locally = linkedImageLucid

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Image Locally (Path Abs File) (L.HtmlT (GroundedUrlT m)) where
  deploy Image Locally i = do
    link <- lift (absFileUrl i)
    linkedImageLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Image Locally (Path Abs Dir) (L.HtmlT (GroundedUrlT m)) where
  deploy Image Locally i = do
    link <- lift (absDirUrl i)
    linkedImageLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Image Locally Location (L.HtmlT (GroundedUrlT m)) where
  deploy Image Locally i = do
    link <- lift (locUrl i)
    linkedImageLucid (printURI link)


-- Blaze

-- Remote

linkedImageBlaze :: T.Text -> HI.MarkupM ()
linkedImageBlaze link =
  H.img H.! A.src (H.toValue link)

instance Deploy Image Remote T.Text HI.MarkupM where
  deploy Image Remote = linkedImageBlaze

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Image Remote (Path Abs File) (AbsoluteUrlT HI.MarkupM) where
  deploy Image Remote i = do
    link <- absFileUrl i
    lift (linkedImageBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Image Remote (Path Abs Dir) (AbsoluteUrlT HI.MarkupM) where
  deploy Image Remote i = do
    link <- absDirUrl i
    lift (linkedImageBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Image Remote Location (AbsoluteUrlT HI.MarkupM) where
  deploy Image Remote i = do
    link <- locUrl i
    lift (linkedImageBlaze (printURI link))


-- Local

instance Deploy Image Locally T.Text HI.MarkupM where
  deploy Image Locally = linkedImageBlaze

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Image Locally (Path Abs File) (GroundedUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- absFileUrl i
    lift (linkedImageBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Image Locally (Path Abs Dir) (GroundedUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- absDirUrl i
    lift (linkedImageBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Image Locally Location (GroundedUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- locUrl i
    lift (linkedImageBlaze (printURI link))


-- JavaScript

-- Remote

linkedJavaScriptLucid :: Monad m => T.Text -> L.HtmlT m ()
linkedJavaScriptLucid link =
  L.script_ [L.src_ link] ("" :: T.Text)

instance ( Monad m
         ) => Deploy JavaScript Remote T.Text (L.HtmlT m) where
  deploy JavaScript Remote = linkedJavaScriptLucid

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy JavaScript Remote (Path Abs File) (L.HtmlT (AbsoluteUrlT m)) where
  deploy JavaScript Remote i = do
    link <- lift (absFileUrl i)
    linkedJavaScriptLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy JavaScript Remote (Path Abs Dir) (L.HtmlT (AbsoluteUrlT m)) where
  deploy JavaScript Remote i = do
    link <- lift (absDirUrl i)
    linkedJavaScriptLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy JavaScript Remote Location (L.HtmlT (AbsoluteUrlT m)) where
  deploy JavaScript Remote i = do
    link <- lift (locUrl i)
    linkedJavaScriptLucid (printURI link)


-- Local

instance ( Monad m
         ) => Deploy JavaScript Locally T.Text (L.HtmlT m) where
  deploy JavaScript Locally = linkedJavaScriptLucid

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy JavaScript Locally (Path Abs File) (L.HtmlT (GroundedUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (absFileUrl i)
    linkedJavaScriptLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy JavaScript Locally (Path Abs Dir) (L.HtmlT (GroundedUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (absDirUrl i)
    linkedJavaScriptLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy JavaScript Locally Location (L.HtmlT (GroundedUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (locUrl i)
    linkedJavaScriptLucid (printURI link)

-- Inline

instance ( Monad m
         ) => Deploy JavaScript Inline T.Text (L.HtmlT m) where
  deploy JavaScript Inline =
    L.script_ []

instance ( Monad m
         ) => Deploy JavaScript Inline LT.Text (L.HtmlT m) where
  deploy JavaScript Inline =
    L.script_ []


-- Blaze

-- Remote

linkedJavaScriptBlaze :: T.Text -> HI.MarkupM ()
linkedJavaScriptBlaze link =
  H.script (H.toHtml ("" :: T.Text)) H.! A.src (H.toValue link)

instance Deploy JavaScript Remote T.Text HI.MarkupM where
  deploy JavaScript Remote = linkedJavaScriptBlaze

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy JavaScript Remote (Path Abs File) (AbsoluteUrlT HI.MarkupM) where
  deploy JavaScript Remote i = do
    link <- absFileUrl i
    lift (linkedJavaScriptBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy JavaScript Remote (Path Abs Dir) (AbsoluteUrlT HI.MarkupM) where
  deploy JavaScript Remote i = do
    link <- absDirUrl i
    lift (linkedJavaScriptBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy JavaScript Remote Location (AbsoluteUrlT HI.MarkupM) where
  deploy JavaScript Remote i = do
    link <- locUrl i
    lift (linkedJavaScriptBlaze (printURI link))


-- Local

instance Deploy JavaScript Locally T.Text HI.MarkupM where
  deploy JavaScript Locally = linkedJavaScriptBlaze

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Path Abs File) (GroundedUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- absFileUrl i
    lift (linkedJavaScriptBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Path Abs Dir) (GroundedUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- absDirUrl i
    lift (linkedJavaScriptBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally Location (GroundedUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- locUrl i
    lift (linkedJavaScriptBlaze (printURI link))

-- Inline

instance Deploy JavaScript Inline T.Text HI.MarkupM where
  deploy JavaScript Inline i =
    H.script (H.toHtml i)

instance Deploy JavaScript Inline LT.Text HI.MarkupM where
  deploy JavaScript Inline i =
    H.script (H.toHtml i)


-- Css

-- Remote

linkedCssLucid :: Monad m => T.Text -> L.HtmlT m ()
linkedCssLucid link =
  L.link_ [ L.rel_ "stylesheet"
          , L.type_ "text/css"
          , L.href_ link
          ]

instance ( Monad m
         ) => Deploy Css Remote T.Text (L.HtmlT m) where
  deploy Css Remote = linkedCssLucid

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Css Remote (Path Abs File) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Css Remote i = do
    link <- lift (absFileUrl i)
    linkedCssLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Css Remote (Path Abs Dir) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Css Remote i = do
    link <- lift (absDirUrl i)
    linkedCssLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy Css Remote Location (L.HtmlT (AbsoluteUrlT m)) where
  deploy Css Remote i = do
    link <- lift (locUrl i)
    linkedCssLucid (printURI link)


-- Local

instance ( Monad m
         ) => Deploy Css Locally T.Text (L.HtmlT m) where
  deploy Css Locally = linkedCssLucid

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Css Locally (Path Abs File) (L.HtmlT (GroundedUrlT m)) where
  deploy Css Locally i = do
    link <- lift (absFileUrl i)
    linkedCssLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Css Locally (Path Abs Dir) (L.HtmlT (GroundedUrlT m)) where
  deploy Css Locally i = do
    link <- lift (absDirUrl i)
    linkedCssLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy Css Locally Location (L.HtmlT (GroundedUrlT m)) where
  deploy Css Locally i = do
    link <- lift (locUrl i)
    linkedCssLucid (printURI link)

-- Inline

instance ( Monad m
         ) => Deploy Css Inline T.Text (L.HtmlT m) where
  deploy Css Inline =
    L.style_ []

instance ( Monad m
         ) => Deploy Css Inline LT.Text (L.HtmlT m) where
  deploy Css Inline =
    L.style_ []

instance ( Monad m
         ) => Deploy Css Inline C.Css (L.HtmlT m) where
  deploy Css Inline i =
    L.style_ [] (C.render i)


-- Blaze

-- Remote

linkedCssBlaze :: T.Text -> HI.MarkupM ()
linkedCssBlaze link =
  H.link H.! A.rel "stylesheet"
         H.! A.type_ "text/css"
         H.! A.href (H.toValue link)

instance Deploy Css Remote T.Text HI.MarkupM where
  deploy Css Remote = linkedCssBlaze

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Css Remote (Path Abs File) (AbsoluteUrlT HI.MarkupM) where
  deploy Css Remote i = do
    link <- absFileUrl i
    lift (linkedCssBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Css Remote (Path Abs Dir) (AbsoluteUrlT HI.MarkupM) where
  deploy Css Remote i = do
    link <- absDirUrl i
    lift (linkedCssBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Css Remote Location (AbsoluteUrlT HI.MarkupM) where
  deploy Css Remote i = do
    link <- locUrl i
    lift (linkedCssBlaze (printURI link))


-- Local

instance Deploy Css Locally T.Text HI.MarkupM where
  deploy Css Locally = linkedCssBlaze

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Css Locally (Path Abs File) (GroundedUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- absFileUrl i
    lift (linkedCssBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Css Locally (Path Abs Dir) (GroundedUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- absDirUrl i
    lift (linkedCssBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy Css Locally Location (GroundedUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- locUrl i
    lift (linkedCssBlaze (printURI link))

-- Inline

instance Deploy Css Inline T.Text HI.MarkupM where
  deploy Css Inline i =
    H.style (H.toHtml i)

instance Deploy Css Inline LT.Text HI.MarkupM where
  deploy Css Inline i =
    H.style (H.toHtml i)

instance Deploy Css Inline C.Css HI.MarkupM where
  deploy Css Inline i =
    H.style (H.toHtml (C.render i))


-- WebComponent instances

-- Remote

linkedWebComponentLucid :: Monad m => T.Text -> L.HtmlT m ()
linkedWebComponentLucid link =
  L.link_ [ L.rel_ "import"
          , L.href_ link
          ]

instance ( Monad m
         ) => Deploy WebComponent Remote T.Text (L.HtmlT m) where
  deploy WebComponent Remote = linkedWebComponentLucid

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy WebComponent Remote (Path Abs File) (L.HtmlT (AbsoluteUrlT m)) where
  deploy WebComponent Remote i = do
    link <- lift (absFileUrl i)
    linkedWebComponentLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy WebComponent Remote (Path Abs Dir) (L.HtmlT (AbsoluteUrlT m)) where
  deploy WebComponent Remote i = do
    link <- lift (absDirUrl i)
    linkedWebComponentLucid (printURI link)

instance ( Monad m
         , MonadUrl (AbsoluteUrlT m)
         ) => Deploy WebComponent Remote Location (L.HtmlT (AbsoluteUrlT m)) where
  deploy WebComponent Remote i = do
    link <- lift (locUrl i)
    linkedWebComponentLucid (printURI link)

-- instance ( Monad m
--          , MonadUrl Abs t (AbsoluteUrlT m)
--          , MonadThrow (AbsoluteUrlT m)
--          , ToLocation s Abs t
--          ) => Deploy WebComponent s (L.HtmlT (AbsoluteUrlT m)) where
--   deploy WebComponent Remote i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (printURI link)


-- Local

instance ( Monad m
         ) => Deploy WebComponent Locally T.Text (L.HtmlT m) where
  deploy WebComponent Locally = linkedWebComponentLucid

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy WebComponent Locally (Path Abs File) (L.HtmlT (GroundedUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (absFileUrl i)
    linkedWebComponentLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy WebComponent Locally (Path Abs Dir) (L.HtmlT (GroundedUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (absDirUrl i)
    linkedWebComponentLucid (printURI link)

instance ( Monad m
         , MonadUrl (GroundedUrlT m)
         ) => Deploy WebComponent Locally Location (L.HtmlT (GroundedUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (locUrl i)
    linkedWebComponentLucid (printURI link)

-- instance ( Monad m
--          , MonadUrl Abs t (GroundedUrlT m)
--          , MonadThrow (GroundedUrlT m)
--          , ToLocation s Abs t
--          ) => Deploy WebComponent Locally s (L.HtmlT (GroundedUrlT m)) where
--   deploy WebComponent Locally i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (printURI link)
--
-- instance ( Monad m
--          , MonadUrl Rel t (RelativeUrlT m)
--          , MonadThrow (RelativeUrlT m)
--          , ToLocation s Rel t
--          ) => Deploy WebComponent Locally s (L.HtmlT (RelativeUrlT m)) where
--   deploy WebComponent Locally i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (printURI link)

-- Blaze

-- Remote

linkedWebComponentBlaze :: T.Text -> HI.MarkupM ()
linkedWebComponentBlaze link =
  H.link H.! A.rel "import"
         H.! A.href (H.toValue link)

instance Deploy WebComponent Remote T.Text HI.MarkupM where
  deploy WebComponent Remote = linkedWebComponentBlaze

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy WebComponent Remote (Path Abs File) (AbsoluteUrlT HI.MarkupM) where
  deploy WebComponent Remote i = do
    link <- absFileUrl i
    lift (linkedWebComponentBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy WebComponent Remote (Path Abs Dir) (AbsoluteUrlT HI.MarkupM) where
  deploy WebComponent Remote i = do
    link <- absDirUrl i
    lift (linkedWebComponentBlaze (printURI link))

instance ( MonadUrl (AbsoluteUrlT HI.MarkupM)
         ) => Deploy WebComponent Remote Location (AbsoluteUrlT HI.MarkupM) where
  deploy WebComponent Remote i = do
    link <- locUrl i
    lift (linkedWebComponentBlaze (printURI link))


-- Local

instance Deploy WebComponent Locally T.Text HI.MarkupM where
  deploy WebComponent Locally = linkedWebComponentBlaze

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Path Abs File) (GroundedUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- absFileUrl i
    lift (linkedWebComponentBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Path Abs Dir) (GroundedUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- absDirUrl i
    lift (linkedWebComponentBlaze (printURI link))

instance ( MonadUrl (GroundedUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally Location (GroundedUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- locUrl i
    lift (linkedWebComponentBlaze (printURI link))
