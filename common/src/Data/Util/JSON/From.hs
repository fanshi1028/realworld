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

import Data.Aeson (FromJSON, Value, withObject, (.:))
import Data.Aeson.Types (Object, Parser)
import Data.HashMap.Strict (keys)
import Relude.Extra (insertWith)

-- | @since 0.1.0.0
-- Wrapping type for making an In 'FromJSON' instance
newtype In a = In a deriving (Show, Generic)

-- | @since 0.1.0.0
-- Helper to override and provide default value when writing 'FromJSON' instance
wrappedParseJSON :: FromJSON a => String -> Text -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> o .: key

-- | @since 0.1.0.0
-- The value is only inserted when the key is not present in the 'Object'
insert' ::
  -- | key
  Text ->
  -- | value
  Value ->
  -- | original Object
  Object ->
  Object
insert' = insertWith (\_ old -> old)

-- | @since 0.1.0.0
-- only specified keys are acceptable
acceptOnlyKeys ::
  -- | list of specified keys
  [Text] ->
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
