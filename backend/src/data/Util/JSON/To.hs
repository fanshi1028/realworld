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
module Util.JSON.To
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

-- | Wrapping newtype for making an "out" ToJSON instance
--
-- @since 0.1.0.0
newtype Out a = Out a deriving (Show, Generic)

-- | Wrap an Value with a key
--
-- @since 0.2.0.0
wrapJSON :: Text -> Value -> Value
wrapJSON key json = Object $ fromList [(key, json)]

-- | Wrap an encoding with a key
--
-- @since 0.1.0.0
wrapEncoding :: Text -> Encoding -> Encoding
wrapEncoding key encoding = wrapObject $ text key >< colon >< encoding

-- | 'ToJSON' instance wrapping helper function for toJSON
--
-- @since 0.2.0.0
wrappedToJSON :: (ToJSON a) => Text -> Out a -> Value
wrappedToJSON key (Out a) = wrapJSON key $ toJSON a

-- | 'ToJSON' instance wrapping helper function for toEncoding
--
-- @since 0.2.0.0
wrappedToEncoding :: (ToJSON a) => Text -> Out a -> Encoding
wrappedToEncoding key (Out a) = wrapEncoding key $ toEncoding a

-- | 'ToJSON' instance wrapping helper function for toJSON (for mult row of data)
--
-- @since 0.2.0.0
multiWrappedWithCountToJSON :: (ToJSON (t a), Foldable t) => Text -> Text -> Out (t a) -> Value
multiWrappedWithCountToJSON key countKey (Out ta) = Object $ fromList [(key, toJSON ta), (countKey, Number $ fromIntegral $ length ta)]

-- | 'ToJSON' instance  wrapping helper function for toEncoding (for mult row of data)
--
-- @since 0.1.0.0
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Text -> Text -> Out (t a) -> Encoding
multiWrappedWithCountToEncoding key countKey (Out ta) =
  wrapObject $
    text key >< colon >< toEncoding ta
      >< comma
      >< text countKey
      >< colon
      >< int (length ta)
