{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Helper
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Utils to tweak 'FromJSON' instances
--
-- @since 0.4.0.0
module Data.Util.JSON.From
  ( -- * Types
    In (..),

    -- * Helpers
    insert',
    wrappedParseJSON,
    acceptOnlyKeys,
  )
where

import Data.Aeson (FromJSON, Key, Value, withObject, (.:))
import Data.Aeson.KeyMap (insert, keys, member)
import Data.Aeson.Types (Object, Parser)

-- | @since 0.1.0.0
-- Wrapping type for making an In 'FromJSON' instance
newtype In a = In a deriving (Show, Generic)

-- | @since 0.4.0.0
-- Helper to override and provide default value when writing 'FromJSON' instance
wrappedParseJSON :: FromJSON a => String -> Key -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> o .: key

-- | @since 0.4.0.0
-- The value is only inserted when the key is not present in the 'Object'
insert' ::
  -- | key
  Key ->
  -- | value
  Value ->
  -- | original Object
  Object ->
  Object
insert' k v km = if member k km then km else insert k v km

-- | @since 0.4.0.0
-- only specified keys are acceptable
acceptOnlyKeys ::
  -- | list of specified keys
  [Key] ->
  -- | error message to append when unacceptable key presented
  String ->
  -- | the Object to check
  Object ->
  Parser ()
acceptOnlyKeys ks errMsg =
  keys
    >>> foldl' (\acc k -> if k `notElem` ks then k : acc else acc) []
    >>> \case
      [] -> pure ()
      unknownFields -> fail $ errMsg ++ show unknownFields
