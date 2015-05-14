{-# LANGUAGE
    ExtendedDefaultRules
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}

module Data.Markup.Library where

import           Data.Markup.Class
import           Data.Markup.Types

import           Data.Url

import qualified Lucid                       as L
import qualified Lucid.Base                  as LBase

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as HI

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT

import           Control.Monad.Trans

data Image = Image deriving (Show, Eq)
data JavaScript = JavaScript deriving (Show, Eq)
data Css = Css deriving (Show, Eq)
data WebComponent = WebComponent deriving (Show, Eq)

-- Lucid instances

-- Images only have local and hosted instances - inline can only be done with
-- js... TODO?

instance Monad m =>
           Deploy Image T.Text (LBase.HtmlT m ()) HostedMarkupM where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance ( Monad m
         , Url T.Text m ) =>
           Deploy Image T.Text (LBase.HtmlT m ()) LocalMarkupM where
  deploy Image i = return $ do
    link <- lift $ plainUrl i
    L.img_ [L.src_ link]

instance Url T.Text m =>
           Deploy Image (QueryString T.Text) (LBase.HtmlT m ()) LocalMarkupM where
  deploy Image i = return $ do
    link <- lift $ queryUrl i
    L.img_ [L.src_ link]

instance ( Monad m
         , Monad m' ) =>
             Deploy Image T.Text (LBase.HtmlT m ()) (HostedMarkupT m') where
  deploy Image i = return $
    L.img_ [L.src_ i]

instance ( Monad m
         , Monad m'
         , Url T.Text m ) =>
             Deploy Image T.Text (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy Image i = return $ do
    link <- lift $ plainUrl i
    L.img_ [L.src_ link]

instance ( Url T.Text m
         , Monad m' ) =>
             Deploy Image (QueryString T.Text) (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy Image i = return $ do
    link <- lift $ queryUrl i
    L.img_ [L.src_ link]

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

instance Url T.Text m =>
           Deploy JavaScript (QueryString T.Text) (LBase.HtmlT m ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    link <- lift $ queryUrl i
    L.script_ [L.src_ link] ("" :: T.Text)

instance Url T.Text m =>
           Deploy JavaScript T.Text (LBase.HtmlT m ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    link <- lift $ plainUrl i
    L.script_ [L.src_ link] ("" :: T.Text)

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

instance ( Url T.Text m
         , Monad m' ) =>
             Deploy JavaScript T.Text (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy JavaScript i = return $ do
    link <- lift $ plainUrl i
    L.script_ [L.src_ link] ("" :: T.Text)

instance ( Url T.Text m
         , Monad m' ) =>
             Deploy JavaScript (QueryString T.Text) (LBase.HtmlT m ()) (LocalMarkupT m') where
  deploy JavaScript i = return $ do
    link <- lift $ queryUrl i
    L.script_ [L.src_ link] ("" :: T.Text)

-- Css instances

instance Monad m =>
  Deploy Css T.Text (LBase.HtmlT m ()) InlineMarkupM where
    deploy Css i = return $
      L.style_ [] i

instance Monad m =>
  Deploy Css LT.Text (LBase.HtmlT m ()) InlineMarkupM where
    deploy Css i = return $
      L.style_ [] i

instance Monad m =>
  Deploy Css T.Text (LBase.HtmlT m ()) HostedMarkupM where
    deploy Css i = return $
      L.link_ [ L.rel_ "stylesheet"
              , L.type_ "text/css"
              , L.href_ i
              ]

instance Url T.Text m =>
  Deploy Css T.Text (LBase.HtmlT m ()) LocalMarkupM where
    deploy Css i = return $ do
      link <- lift $ plainUrl i
      L.link_ [ L.rel_ "stylesheet"
              , L.type_ "text/css"
              , L.href_ link
              ]

instance Url T.Text m =>
  Deploy Css (QueryString T.Text) (LBase.HtmlT m ()) LocalMarkupM where
    deploy Css i = return $ do
      link <- lift $ queryUrl i
      L.link_ [ L.rel_ "stylesheet"
              , L.type_ "text/css"
              , L.href_ link
              ]

instance ( Monad m
         , Monad m' ) =>
             Deploy Css T.Text (LBase.HtmlT m ()) (InlineMarkupT m') where
               deploy Css i = return $
                 L.style_ [] i

instance ( Monad m
         , Monad m' ) =>
             Deploy Css LT.Text (LBase.HtmlT m ()) (InlineMarkupT m') where
               deploy Css i = return $
                 L.style_ [] i

instance ( Monad m
         , Monad m' ) =>
             Deploy Css T.Text (LBase.HtmlT m ()) (HostedMarkupT m') where
               deploy Css i = return $
                 L.link_ [ L.rel_ "stylesheet"
                         , L.type_ "text/css"
                         , L.href_ i
                         ]

instance ( Url T.Text m
         , Monad m' ) =>
             Deploy Css T.Text (LBase.HtmlT m ()) (LocalMarkupT m') where
               deploy Css i = return $ do
                 link <- lift $ plainUrl i
                 L.link_ [ L.rel_ "stylesheet"
                         , L.type_ "text/css"
                         , L.href_ link
                       ]

instance ( Url T.Text m
         , Monad m' ) =>
             Deploy Css (QueryString T.Text) (LBase.HtmlT m ()) (LocalMarkupT m') where
               deploy Css i = return $ do
                 link <- lift $ queryUrl i
                 L.link_ [ L.rel_ "stylesheet"
                         , L.type_ "text/css"
                         , L.href_ link
                       ]

-- WebComponent instances

instance Monad m =>
  Deploy WebComponent T.Text (LBase.HtmlT m ()) HostedMarkupM where
    deploy WebComponent i = return $
      L.link_ [ L.rel_ "import"
              , L.href_ i
              ]

instance ( Monad m
         , Url T.Text m ) =>
  Deploy WebComponent T.Text (LBase.HtmlT m ()) LocalMarkupM where
    deploy WebComponent i = return $ do
      link <- lift $ plainUrl i
      L.link_ [ L.rel_ "import"
              , L.href_ link
              ]

instance ( Monad m
         , Url T.Text m ) =>
  Deploy WebComponent (QueryString T.Text) (LBase.HtmlT m ()) LocalMarkupM where
    deploy WebComponent i = return $ do
      link <- lift $ queryUrl i
      L.link_ [ L.rel_ "import"
              , L.href_ link
              ]

instance ( Monad m
         , Monad m' ) =>
  Deploy WebComponent T.Text (LBase.HtmlT m ()) (HostedMarkupT m') where
    deploy WebComponent i = return $
      L.link_ [ L.rel_ "import"
              , L.href_ i
              ]

instance ( Monad m
         , Monad m'
         , Url T.Text m ) =>
  Deploy WebComponent T.Text (LBase.HtmlT m ()) (LocalMarkupT m') where
    deploy WebComponent i = return $ do
      link <- lift $ plainUrl i
      L.link_ [ L.rel_ "import"
              , L.href_ link
              ]

instance ( Monad m
         , Monad m'
         , Url T.Text m ) =>
  Deploy WebComponent (QueryString T.Text) (LBase.HtmlT m ()) (LocalMarkupT m') where
    deploy WebComponent i = return $ do
      link <- lift $ queryUrl i
      L.link_ [ L.rel_ "import"
              , L.href_ link
              ]

-- Blaze-html instances


instance H.ToValue input =>
           Deploy Image input (HI.MarkupM ()) HostedMarkupM where
  deploy Image i = return $
    H.img H.! A.src (H.toValue i)

instance Url T.Text HI.MarkupM =>
           Deploy Image T.Text (HI.MarkupM ()) LocalMarkupM where
  deploy Image i = return $ do
    link <- plainUrl i
    H.img H.! A.src (H.toValue link)

instance Url T.Text HI.MarkupM =>
           Deploy Image (QueryString T.Text) (HI.MarkupM ()) LocalMarkupM where
  deploy Image i = return $ do
    link <- queryUrl i
    H.img H.! A.src (H.toValue link)

instance ( H.ToValue input
         , Monad m ) =>
             Deploy Image input (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image i = return $
    H.img H.! A.src (H.toValue i)

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy Image T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = return $ do
    link <- plainUrl i
    H.img H.! A.src (H.toValue link)

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy Image (QueryString T.Text) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = return $ do
    link <- queryUrl i
    H.img H.! A.src (H.toValue link)



instance H.ToMarkup input =>
           Deploy JavaScript input (HI.MarkupM ()) InlineMarkupM where
  deploy JavaScript i = return $
    H.script (H.toMarkup i)

instance H.ToValue input =>
           Deploy JavaScript input (HI.MarkupM ()) HostedMarkupM where
  deploy JavaScript i = return $
    (H.script H.! A.src (H.toValue i)) HI.Empty

instance Url T.Text HI.MarkupM =>
           Deploy JavaScript T.Text (HI.MarkupM ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    link <- plainUrl i
    (H.script H.! A.src (H.toValue link)) HI.Empty

instance Url T.Text HI.MarkupM =>
           Deploy JavaScript (QueryString T.Text) (HI.MarkupM ()) LocalMarkupM where
  deploy JavaScript i = return $ do
    link <- queryUrl i
    (H.script H.! A.src (H.toValue link)) HI.Empty

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

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy JavaScript T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = return $ do
    link <- plainUrl i
    (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy JavaScript (QueryString T.Text) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = return $ do
    link <- queryUrl i
    (H.script H.! A.src (H.toValue link)) HI.Empty



instance H.ToMarkup input =>
           Deploy Css input (HI.MarkupM ()) InlineMarkupM where
  deploy Css i = return $
    H.style (H.toMarkup i)

instance H.ToValue input =>
           Deploy Css input (HI.MarkupM ()) HostedMarkupM where
  deploy Css i = return $
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue i)

instance Url T.Text HI.MarkupM =>
           Deploy Css T.Text (HI.MarkupM ()) LocalMarkupM where
  deploy Css i = return $ do
    link <- plainUrl i
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue link)

instance Url T.Text HI.MarkupM =>
           Deploy Css (QueryString T.Text) (HI.MarkupM ()) LocalMarkupM where
  deploy Css i = return $ do
    link <- queryUrl i
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue link)

instance ( H.ToMarkup input
         , Monad m ) =>
             Deploy Css input (HI.MarkupM ()) (InlineMarkupT m) where
  deploy Css i = return $
    H.style (H.toMarkup i)

instance ( H.ToValue input
         , Monad m ) =>
             Deploy Css input (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Css i = return $
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue i)

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy Css T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = return $ do
    link <- plainUrl i
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue link)

instance ( Url T.Text HI.MarkupM
         , Monad m ) =>
             Deploy Css (QueryString T.Text) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = return $ do
    link <- queryUrl i
    H.link H.! A.rel "stylesheet"
            H.! A.type_ "text/css"
            H.! A.href (H.toValue link)

instance H.ToValue input =>
           Deploy WebComponent input (HI.MarkupM ()) HostedMarkupM where
  deploy WebComponent i = return $
    H.link H.! A.rel "import"
           H.! A.href (H.toValue i)

instance Url T.Text HI.MarkupM =>
           Deploy WebComponent T.Text (HI.MarkupM ()) LocalMarkupM where
  deploy WebComponent i = return $ do
    link <- plainUrl i
    H.link H.! A.rel "import"
           H.! A.href (H.toValue link)

instance Url T.Text HI.MarkupM =>
           Deploy WebComponent (QueryString T.Text) (HI.MarkupM ()) LocalMarkupM where
  deploy WebComponent i = return $ do
    link <- queryUrl i
    H.link H.! A.rel "import"
           H.! A.href (H.toValue link)

instance ( H.ToValue input
         , Monad m'
         ) =>
           Deploy WebComponent input (HI.MarkupM ()) (HostedMarkupT m') where
  deploy WebComponent i = return $
    H.link H.! A.rel "import"
           H.! A.href (H.toValue i)

instance ( Monad m'
         , Url T.Text HI.MarkupM
         ) =>
           Deploy WebComponent T.Text (HI.MarkupM ()) (LocalMarkupT m') where
  deploy WebComponent i = return $ do
    link <- plainUrl i
    H.link H.! A.rel "import"
           H.! A.href (H.toValue link)

instance ( Monad m'
         , Url T.Text HI.MarkupM
         ) =>
           Deploy WebComponent (QueryString T.Text) (HI.MarkupM ()) (LocalMarkupT m') where
  deploy WebComponent i = return $ do
    link <- queryUrl i
    H.link H.! A.rel "import"
           H.! A.href (H.toValue link)
