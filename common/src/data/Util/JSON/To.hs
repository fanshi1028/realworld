{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Helper
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Utils to tweak 'ToJSON' instances
--
-- @since 0.4.0.0
module Data.Util.JSON.To
  ( -- * Types
    Out (..),

    -- * Helpers

    -- ** toJSON
    wrapJSON,
    wrappedToJSON,
    multiWrappedWithCountToJSON,

    -- ** toEncoding
    wrapEncoding,
    wrappedToEncoding,
    multiWrappedWithCountToEncoding,
  )
where

import Data.Aeson (Encoding, ToJSON (toEncoding), Value (Number, Object), toJSON)
import Data.Aeson.Encoding (int, text)
import Data.Aeson.Encoding.Internal (colon, comma, wrapObject, (><))

-- | @since 0.1.0.0
-- Wrapping newtype for making an out 'ToJSON' instance
newtype Out a = Out a deriving (Show, Generic)

-- | @since 0.2.0.0
-- Wrap an Value with a key
wrapJSON :: Text -> Value -> Value
wrapJSON key json = Object $ fromList [(key, json)]

-- | @since 0.1.0.0
-- Wrap an encoding with a key
wrapEncoding :: Text -> Encoding -> Encoding
wrapEncoding key encoding = wrapObject $ text key >< colon >< encoding

-- | @since 0.2.0.0
-- 'ToJSON' instance wrapping helper function for toJSON
wrappedToJSON :: (ToJSON a) => Text -> Out a -> Value
wrappedToJSON key (Out a) = wrapJSON key $ toJSON a

-- | @since 0.2.0.0
-- 'ToJSON' instance wrapping helper function for toEncoding
wrappedToEncoding :: (ToJSON a) => Text -> Out a -> Encoding
wrappedToEncoding key (Out a) = wrapEncoding key $ toEncoding a

-- | @since 0.2.0.0
-- 'ToJSON' instance wrapping helper function for toJSON (for mult row of data)
multiWrappedWithCountToJSON :: (ToJSON (t a), Foldable t) => Text -> Text -> Out (t a) -> Value
multiWrappedWithCountToJSON key countKey (Out ta) = Object $ fromList [(key, toJSON ta), (countKey, Number $ fromIntegral $ length ta)]

-- | @since 0.1.0.0
-- 'ToJSON' instance  wrapping helper function for toEncoding (for mult row of data)
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Text -> Text -> Out (t a) -> Encoding
multiWrappedWithCountToEncoding key countKey (Out ta) =
  wrapObject $
    text key >< colon >< toEncoding ta
      >< comma
      >< text countKey
      >< colon
      >< int (length ta)
