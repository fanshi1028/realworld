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

import Data.Aeson (Encoding, Key, ToJSON (toEncoding), Value (Number, Object), toJSON)
import Data.Aeson.Encoding (int, text)
import Data.Aeson.Encoding.Internal (colon, comma, wrapObject, (><))
import qualified Data.Aeson.Key as K (toText)

-- | @since 0.1.0.0
-- Wrapping newtype for making an out 'ToJSON' instance
newtype Out a = Out
  { -- | @since 0.4.0.0
    unOut :: a
  }
  deriving (Show, Generic)

-- | @since 0.4.0.0
-- Wrap an Value with a key
wrapJSON :: Key -> Value -> Value
wrapJSON key json = Object $ fromList [(key, json)]

-- | @since 0.4.0.0
-- Wrap an encoding with a key
wrapEncoding :: Key -> Encoding -> Encoding
wrapEncoding key encoding = wrapObject $ text (K.toText key) >< colon >< encoding

-- | @since 0.4.0.0
-- 'ToJSON' instance wrapping helper function for toJSON
wrappedToJSON :: (ToJSON a) => Key -> Out a -> Value
wrappedToJSON key (Out a) = wrapJSON key $ toJSON a

-- | @since 0.4.0.0
-- 'ToJSON' instance wrapping helper function for toEncoding
wrappedToEncoding :: (ToJSON a) => Key -> Out a -> Encoding
wrappedToEncoding key (Out a) = wrapEncoding key $ toEncoding a

-- | @since 0.4.0.0
-- 'ToJSON' instance wrapping helper function for toJSON (for mult row of data)
multiWrappedWithCountToJSON :: (ToJSON (t a), Foldable t) => Key -> Key -> Out (t a) -> Value
multiWrappedWithCountToJSON key countKey (Out ta) = Object $ fromList [(key, toJSON ta), (countKey, Number $ fromIntegral $ length ta)]

-- | @since 0.4.0.0
-- 'ToJSON' instance  wrapping helper function for toEncoding (for mult row of data)
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Key -> Key -> Out (t a) -> Encoding
multiWrappedWithCountToEncoding key countKey (Out ta) =
  wrapObject $
    text (K.toText key) >< colon >< toEncoding ta
      >< comma
      >< text (K.toText countKey)
      >< colon
      >< int (length ta)
