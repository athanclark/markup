{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , UndecidableInstances
  , ExtendedDefaultRules
  , MultiParamTypeClasses
  #-}

module Data.Markup.Library where

import           Data.Markup.Class
import           Data.Markup.Types

import           Data.Url
import           Path.Extended

import qualified Lucid                       as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as HI
import qualified Clay                        as C

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import           Control.Monad.Trans


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
  deploy Image Remote link = linkedImageLucid link

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy Image Remote (Path Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Image Remote i = do
    link <- lift (pathUrl i)
    linkedImageLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy Image Remote (Location Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Image Remote i = do
    link <- lift (locUrl i)
    linkedImageLucid (T.pack link)


-- Local

instance ( Monad m
         ) => Deploy Image Locally T.Text (L.HtmlT m) where
  deploy Image Locally link = linkedImageLucid link

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy Image Locally (Path Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy Image Locally i = do
    link <- lift (pathUrl i)
    linkedImageLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy Image Locally (Path Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy Image Locally i = do
    link <- lift (pathUrl i)
    linkedImageLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy Image Locally (Location Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy Image Locally i = do
    link <- lift (locUrl i)
    linkedImageLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy Image Locally (Location Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy Image Locally i = do
    link <- lift (locUrl i)
    linkedImageLucid (T.pack link)


-- Blaze

-- Remote

linkedImageBlaze :: T.Text -> HI.MarkupM ()
linkedImageBlaze link =
  H.img H.! A.src (H.toValue link)

instance Deploy Image Remote T.Text (HI.MarkupM) where
  deploy Image Remote link = linkedImageBlaze link

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Image Remote (Path Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy Image Remote i = do
    link <- pathUrl i
    lift (linkedImageBlaze (T.pack link))

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Image Remote (Location Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy Image Remote i = do
    link <- locUrl i
    lift (linkedImageBlaze (T.pack link))


-- Local

instance Deploy Image Locally T.Text (HI.MarkupM) where
  deploy Image Locally link = linkedImageBlaze link

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy Image Locally (Path Abs t) (GroundedUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- pathUrl i
    lift (linkedImageBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy Image Locally (Path Rel t) (RelativeUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- pathUrl i
    lift (linkedImageBlaze (T.pack link))

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy Image Locally (Location Abs t) (GroundedUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- locUrl i
    lift (linkedImageBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy Image Locally (Location Rel t) (RelativeUrlT HI.MarkupM) where
  deploy Image Locally i = do
    link <- locUrl i
    lift (linkedImageBlaze (T.pack link))


-- JavaScript

-- Remote

linkedJavaScriptLucid :: Monad m => T.Text -> L.HtmlT m ()
linkedJavaScriptLucid link =
  L.script_ [L.src_ link] ("" :: T.Text)

instance ( Monad m
         ) => Deploy JavaScript Remote T.Text (L.HtmlT m) where
  deploy JavaScript Remote link = linkedJavaScriptLucid link

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy JavaScript Remote (Path Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy JavaScript Remote i = do
    link <- lift (pathUrl i)
    linkedJavaScriptLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy JavaScript Remote (Location Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy JavaScript Remote i = do
    link <- lift (locUrl i)
    linkedJavaScriptLucid (T.pack link)


-- Local

instance ( Monad m
         ) => Deploy JavaScript Locally T.Text (L.HtmlT m) where
  deploy JavaScript Locally link = linkedJavaScriptLucid link

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy JavaScript Locally (Path Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (pathUrl i)
    linkedJavaScriptLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy JavaScript Locally (Path Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (pathUrl i)
    linkedJavaScriptLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy JavaScript Locally (Location Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (locUrl i)
    linkedJavaScriptLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy JavaScript Locally (Location Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy JavaScript Locally i = do
    link <- lift (locUrl i)
    linkedJavaScriptLucid (T.pack link)

-- Inline

instance ( Monad m
         ) => Deploy JavaScript Inline T.Text (L.HtmlT m) where
  deploy JavaScript Inline i =
    L.script_ [] i

instance ( Monad m
         ) => Deploy JavaScript Inline LT.Text (L.HtmlT m) where
  deploy JavaScript Inline i =
    L.script_ [] i


-- Blaze

-- Remote

linkedJavaScriptBlaze :: T.Text -> HI.MarkupM ()
linkedJavaScriptBlaze link =
  H.script (H.toHtml ("" :: T.Text)) H.! (A.src $ H.toValue link)

instance Deploy JavaScript Remote T.Text (HI.MarkupM) where
  deploy JavaScript Remote link = linkedJavaScriptBlaze link

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy JavaScript Remote (Path Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy JavaScript Remote i = do
    link <- pathUrl i
    lift (linkedJavaScriptBlaze (T.pack link))

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy JavaScript Remote (Location Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy JavaScript Remote i = do
    link <- locUrl i
    lift (linkedJavaScriptBlaze (T.pack link))


-- Local

instance Deploy JavaScript Locally T.Text (HI.MarkupM) where
  deploy JavaScript Locally link = linkedJavaScriptBlaze link

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Path Abs t) (GroundedUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- pathUrl i
    lift (linkedJavaScriptBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Path Rel t) (RelativeUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- pathUrl i
    lift (linkedJavaScriptBlaze (T.pack link))

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Location Abs t) (GroundedUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- locUrl i
    lift (linkedJavaScriptBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy JavaScript Locally (Location Rel t) (RelativeUrlT HI.MarkupM) where
  deploy JavaScript Locally i = do
    link <- locUrl i
    lift (linkedJavaScriptBlaze (T.pack link))

-- Inline

instance Deploy JavaScript Inline T.Text (HI.MarkupM) where
  deploy JavaScript Inline i =
    H.script (H.toHtml i)

instance Deploy JavaScript Inline LT.Text (HI.MarkupM) where
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
  deploy Css Remote link = linkedCssLucid link

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy Css Remote (Path Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Css Remote i = do
    link <- lift (pathUrl i)
    linkedCssLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy Css Remote (Location Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy Css Remote i = do
    link <- lift (locUrl i)
    linkedCssLucid (T.pack link)


-- Local

instance ( Monad m
         ) => Deploy Css Locally T.Text (L.HtmlT m) where
  deploy Css Locally link = linkedCssLucid link

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy Css Locally (Path Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy Css Locally i = do
    link <- lift (pathUrl i)
    linkedCssLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy Css Locally (Path Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy Css Locally i = do
    link <- lift (pathUrl i)
    linkedCssLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy Css Locally (Location Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy Css Locally i = do
    link <- lift (locUrl i)
    linkedCssLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy Css Locally (Location Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy Css Locally i = do
    link <- lift (locUrl i)
    linkedCssLucid (T.pack link)

-- Inline

instance ( Monad m
         ) => Deploy Css Inline T.Text (L.HtmlT m) where
  deploy Css Inline i =
    L.style_ [] i

instance ( Monad m
         ) => Deploy Css Inline LT.Text (L.HtmlT m) where
  deploy Css Inline i =
    L.style_ [] i

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

instance Deploy Css Remote T.Text (HI.MarkupM) where
  deploy Css Remote link = linkedCssBlaze link

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Css Remote (Path Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy Css Remote i = do
    link <- pathUrl i
    lift (linkedCssBlaze (T.pack link))

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy Css Remote (Location Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy Css Remote i = do
    link <- locUrl i
    lift (linkedCssBlaze (T.pack link))


-- Local

instance Deploy Css Locally T.Text (HI.MarkupM) where
  deploy Css Locally link = linkedCssBlaze link

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy Css Locally (Path Abs t) (GroundedUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- pathUrl i
    lift (linkedCssBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy Css Locally (Path Rel t) (RelativeUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- pathUrl i
    lift (linkedCssBlaze (T.pack link))

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy Css Locally (Location Abs t) (GroundedUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- locUrl i
    lift (linkedCssBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy Css Locally (Location Rel t) (RelativeUrlT HI.MarkupM) where
  deploy Css Locally i = do
    link <- locUrl i
    lift (linkedCssBlaze (T.pack link))

-- Inline

instance Deploy Css Inline T.Text (HI.MarkupM) where
  deploy Css Inline i =
    H.style (H.toHtml i)

instance Deploy Css Inline LT.Text (HI.MarkupM) where
  deploy Css Inline i =
    H.style (H.toHtml i)

instance Deploy Css Inline C.Css (HI.MarkupM) where
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
  deploy WebComponent Remote link = linkedWebComponentLucid link

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy WebComponent Remote (Path Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy WebComponent Remote i = do
    link <- lift (pathUrl i)
    linkedWebComponentLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (AbsoluteUrlT m)
         ) => Deploy WebComponent Remote (Location Abs t) (L.HtmlT (AbsoluteUrlT m)) where
  deploy WebComponent Remote i = do
    link <- lift (locUrl i)
    linkedWebComponentLucid (T.pack link)

-- instance ( Monad m
--          , MonadUrl Abs t (AbsoluteUrlT m)
--          , MonadThrow (AbsoluteUrlT m)
--          , ToLocation s Abs t
--          ) => Deploy WebComponent s (L.HtmlT (AbsoluteUrlT m)) where
--   deploy WebComponent Remote i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (T.pack link)


-- Local

instance ( Monad m
         ) => Deploy WebComponent Locally T.Text (L.HtmlT m) where
  deploy WebComponent Locally link = linkedWebComponentLucid link

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy WebComponent Locally (Path Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (pathUrl i)
    linkedWebComponentLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy WebComponent Locally (Path Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (pathUrl i)
    linkedWebComponentLucid (T.pack link)

instance ( Monad m
         , MonadUrl Abs t (GroundedUrlT m)
         ) => Deploy WebComponent Locally (Location Abs t) (L.HtmlT (GroundedUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (locUrl i)
    linkedWebComponentLucid (T.pack link)

instance ( Monad m
         , MonadUrl Rel t (RelativeUrlT m)
         ) => Deploy WebComponent Locally (Location Rel t) (L.HtmlT (RelativeUrlT m)) where
  deploy WebComponent Locally i = do
    link <- lift (locUrl i)
    linkedWebComponentLucid (T.pack link)

-- instance ( Monad m
--          , MonadUrl Abs t (GroundedUrlT m)
--          , MonadThrow (GroundedUrlT m)
--          , ToLocation s Abs t
--          ) => Deploy WebComponent Locally s (L.HtmlT (GroundedUrlT m)) where
--   deploy WebComponent Locally i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (T.pack link)
--
-- instance ( Monad m
--          , MonadUrl Rel t (RelativeUrlT m)
--          , MonadThrow (RelativeUrlT m)
--          , ToLocation s Rel t
--          ) => Deploy WebComponent Locally s (L.HtmlT (RelativeUrlT m)) where
--   deploy WebComponent Locally i = do
--     i' <- lift (toLocation i)
--     link <- lift (locUrl i')
--     linkedWebComponentLucid (T.pack link)

-- Blaze

-- Remote

linkedWebComponentBlaze :: T.Text -> HI.MarkupM ()
linkedWebComponentBlaze link =
  H.link H.! A.rel "import"
         H.! A.href (H.toValue link)

instance Deploy WebComponent Remote T.Text (HI.MarkupM) where
  deploy WebComponent Remote link = linkedWebComponentBlaze link

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy WebComponent Remote (Path Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy WebComponent Remote i = do
    link <- pathUrl i
    lift (linkedWebComponentBlaze (T.pack link))

instance ( MonadUrl Abs t (AbsoluteUrlT HI.MarkupM)
         ) => Deploy WebComponent Remote (Location Abs t) (AbsoluteUrlT HI.MarkupM) where
  deploy WebComponent Remote i = do
    link <- locUrl i
    lift (linkedWebComponentBlaze (T.pack link))


-- Local

instance Deploy WebComponent Locally T.Text (HI.MarkupM) where
  deploy WebComponent Locally link = linkedWebComponentBlaze link

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Path Abs t) (GroundedUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- pathUrl i
    lift (linkedWebComponentBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Path Rel t) (RelativeUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- pathUrl i
    lift (linkedWebComponentBlaze (T.pack link))

instance ( MonadUrl Abs t (GroundedUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Location Abs t) (GroundedUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- locUrl i
    lift (linkedWebComponentBlaze (T.pack link))

instance ( MonadUrl Rel t (RelativeUrlT HI.MarkupM)
         ) => Deploy WebComponent Locally (Location Rel t) (RelativeUrlT HI.MarkupM) where
  deploy WebComponent Locally i = do
    link <- locUrl i
    lift (linkedWebComponentBlaze (T.pack link))

