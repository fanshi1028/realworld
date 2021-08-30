{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Utils to tweak ToJSON instances
--
-- @since 0.1.0.0
module Domain.Util.JSON.To
  ( -- * Types
    Out (..),

    -- * Helpers
    wrapEncoding,
    wrappedToEncoding,
    multiWrappedWithCountToEncoding,
  )
where

import Data.Aeson (Encoding, GToJSON', ToJSON (toEncoding), Zero, defaultOptions, genericToEncoding)
import Data.Aeson.Encoding (int, text)
import Data.Aeson.Encoding.Internal (colon, comma, wrapObject, (><))
import GHC.Generics (Generic (Rep))

-- | Wrapping newtype for making an "out" ToJSON instance
--
-- @since 0.1.0.0
newtype Out a = Out a deriving (Show, Generic)

-- | Wrap an encoding with a key
--
-- @since 0.1.0.0
wrapEncoding :: Text -> Encoding -> Encoding
wrapEncoding key encoding = wrapObject $ text key >< colon >< encoding

-- | 'ToJSON' instance wrapping helper function
--
-- @since 0.1.0.0
wrappedToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => Text -> a -> Encoding
wrappedToEncoding key a = wrapEncoding key $ genericToEncoding defaultOptions a

-- | 'ToJSON' instance  wrapping helper function (for mult row of data)
--
-- @since 0.1.0.0
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Text -> Text -> t a -> Encoding
multiWrappedWithCountToEncoding key countKey a =
  wrapObject $
    text key >< colon >< toEncoding a
      >< comma
      >< text countKey
      >< colon
      >< int (length a)
