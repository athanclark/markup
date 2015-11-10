{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Data.Markup.Class where


-- | Overload assets and their markup library, over some deployment
class Deploy symbol strategy input result where
  deploy :: symbol -> strategy -> input -> result
