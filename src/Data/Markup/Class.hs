{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Data.Markup.Class where


-- | Overload assets and their markup library, over some deployment
class Deploy symbol strategy input m where
  deploy :: symbol -> strategy -> input -> m ()
