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
import qualified Lucid.Base                  as LBase

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as HI

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT


data Image        = Image        deriving (Show, Eq)
data JavaScript   = JavaScript   deriving (Show, Eq)
data Css          = Css          deriving (Show, Eq)
data WebComponent = WebComponent deriving (Show, Eq)


-- Images

instance ( Monad m
         ) => Deploy Image T.Text (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Image i =
    return $ L.img_ [L.src_ i]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Path b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- pathUrl i
    return $ L.img_ [L.src_ (T.pack link)]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Location b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- locUrl i
    return $ L.img_ [L.src_ (T.pack link)]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Image s (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- symbolUrl i
    return $ L.img_ [L.src_ (T.pack link)]

instance ( Monad m
         ) => Deploy Image T.Text (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Image i =
    return $ L.img_ [L.src_ i]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Path b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- pathUrl i
    return $ L.img_ [L.src_ (T.pack link)]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Location b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- locUrl i
    return $ L.img_ [L.src_ (T.pack link)]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Image s (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- symbolUrl i
    return $ L.img_ [L.src_ (T.pack link)]


-- Blaze

instance ( Monad m
         ) => Deploy Image T.Text (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image link =
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Path b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- pathUrl i
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Location b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- locUrl i
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Image s (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Image i = do
    link <- symbolUrl i
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         ) => Deploy Image T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image link =
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Path b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- pathUrl i
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Image (Location b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- locUrl i
    return $ H.img H.! A.src (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Image s (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Image i = do
    link <- symbolUrl i
    return $ H.img H.! A.src (H.toValue link)


-- JavaScript

instance ( Monad m
         ) => Deploy JavaScript T.Text (LBase.HtmlT m ()) (InlineMarkupT m) where
  deploy JavaScript i =
    return $ L.script_ [] i

instance ( Monad m
         ) => Deploy JavaScript LT.Text (LBase.HtmlT m ()) (InlineMarkupT m) where
  deploy JavaScript i =
    return $ L.script_ [] i

instance ( Monad m
         ) => Deploy JavaScript T.Text (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy JavaScript link =
    return $ L.script_ [L.src_ link] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Path b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- pathUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Location b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- locUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy JavaScript s (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- symbolUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

instance ( Monad m
         ) => Deploy JavaScript T.Text (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy JavaScript link =
    return $ L.script_ [L.src_ link] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Path b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- pathUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Location b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- locUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy JavaScript s (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- symbolUrl i
    return $ L.script_ [L.src_ (T.pack link)] ("" :: T.Text)

-- Blaze

instance ( Monad m
         ) => Deploy JavaScript T.Text (HI.MarkupM ()) (InlineMarkupT m) where
  deploy JavaScript i =
    return $ H.script (H.toMarkup i)

instance ( Monad m
         ) => Deploy JavaScript LT.Text (HI.MarkupM ()) (InlineMarkupT m) where
  deploy JavaScript i =
    return $ H.script (H.toMarkup i)

instance ( Monad m
         ) => Deploy JavaScript T.Text (HI.MarkupM ()) (HostedMarkupT m) where
  deploy JavaScript i =
    return $ (H.script H.! A.src (H.toValue i)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Path b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- pathUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Location b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- locUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy JavaScript s (HI.MarkupM ()) (HostedMarkupT m) where
  deploy JavaScript i = do
    link <- symbolUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Monad m
         ) => Deploy JavaScript T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i =
    return $ (H.script H.! A.src (H.toValue i)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Path b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- pathUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         ) => Deploy JavaScript (Location b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- locUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy JavaScript s (HI.MarkupM ()) (LocalMarkupT m) where
  deploy JavaScript i = do
    link <- symbolUrl i
    return $ (H.script H.! A.src (H.toValue link)) HI.Empty


-- Css instances

instance ( Monad m
         ) => Deploy Css T.Text (LBase.HtmlT m ()) (InlineMarkupT m) where
  deploy Css link =
    return $ L.style_ [] link

instance ( Monad m
         ) => Deploy Css LT.Text (LBase.HtmlT m ()) (InlineMarkupT m) where
  deploy Css link =
    return $ L.style_ [] link

instance ( Monad m
         ) => Deploy Css T.Text (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Css link =
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ link
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Path b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- pathUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Location b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- locUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Css s (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- symbolUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         ) => Deploy Css T.Text (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Css link =
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ link
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Path b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- pathUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Location b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- locUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Css s (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- symbolUrl i
    return $ L.link_ [ L.rel_ "stylesheet"
                     , L.type_ "text/css"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         ) => Deploy Css T.Text (HI.MarkupM ()) (InlineMarkupT m) where
  deploy Css i =
    return $ H.style (H.toMarkup i)

instance ( Monad m
         ) => Deploy Css LT.Text (HI.MarkupM ()) (InlineMarkupT m) where
  deploy Css i =
    return $ H.style (H.toMarkup i)

instance ( Monad m
         ) => Deploy Css T.Text (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Css i = return $
    H.link H.! A.rel "stylesheet"
           H.! A.type_ "text/css"
           H.! A.href (H.toValue i)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Path b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- pathUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Location b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- locUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Css s (HI.MarkupM ()) (HostedMarkupT m) where
  deploy Css i = do
    link <- symbolUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)

instance ( Monad m
         ) => Deploy Css T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = return $
    H.link H.! A.rel "stylesheet"
           H.! A.type_ "text/css"
           H.! A.href (H.toValue i)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Path b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- pathUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy Css (Location b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- locUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy Css s (HI.MarkupM ()) (LocalMarkupT m) where
  deploy Css i = do
    link <- symbolUrl i
    return $ H.link H.! A.rel "stylesheet"
                    H.! A.type_ "text/css"
                    H.! A.href (H.toValue link)


-- WebComponent instances

instance ( Monad m
         ) => Deploy WebComponent T.Text (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy WebComponent link =
    return $ L.link_ [ L.rel_ "import"
                     , L.href_ link
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Path b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- pathUrl i
    return $ L.link_ [ L.rel_ "import"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Location b t) (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- locUrl i
    return $ L.link_ [ L.rel_ "import"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy WebComponent s (LBase.HtmlT m ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- symbolUrl i
    return $ L.link_ [ L.rel_ "import"
                     , L.href_ (T.pack link)
                     ]

instance ( Monad m
         ) => Deploy WebComponent T.Text (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy WebComponent link =
    return $ L.link_ [ L.rel_ "import"
                     , L.href_ link
                     ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Path b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- pathUrl i
    return $ do L.link_ [ L.rel_ "import"
                        , L.href_ (T.pack link)
                        ]

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Location b t) (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- locUrl i
    return $ do L.link_ [ L.rel_ "import"
                        , L.href_ (T.pack link)
                        ]

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy WebComponent s (LBase.HtmlT m ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- symbolUrl i
    return $ do L.link_ [ L.rel_ "import"
                        , L.href_ (T.pack link)
                        ]


-- Blaze

instance ( Monad m
         ) => Deploy WebComponent T.Text (HI.MarkupM ()) (HostedMarkupT m) where
  deploy WebComponent i =
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue i)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Path b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- pathUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Location b t) (HI.MarkupM ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- locUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy WebComponent s (HI.MarkupM ()) (HostedMarkupT m) where
  deploy WebComponent i = do
    link <- symbolUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)

instance ( Monad m
         ) => Deploy WebComponent T.Text (HI.MarkupM ()) (LocalMarkupT m) where
  deploy WebComponent i =
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue i)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Path b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- pathUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         ) => Deploy WebComponent (Location b t) (HI.MarkupM ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- locUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)

instance ( Monad m
         , MonadUrl b m
         , ToLocation s b t
         ) => Deploy WebComponent s (HI.MarkupM ()) (LocalMarkupT m) where
  deploy WebComponent i = do
    link <- symbolUrl i
    return $ H.link H.! A.rel "import"
                    H.! A.href (H.toValue link)
