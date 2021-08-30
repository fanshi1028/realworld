{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Utils to tweak FromJSON instances
--
-- @since 0.1.0.0
module Domain.Util.JSON.From
  ( -- * Types
    In (..),

    -- * Helpers
    insert',
    wrappedParseJSON,
    filterKeysParseJSON,
  )
where

import Data.Aeson (FromJSON (parseJSON), Value (Null, Object), withObject, (.:), (<?>))
import Data.Aeson.Types (JSONPathElement (Key), Object, Parser)
import Data.HashMap.Strict (mapWithKey)
import Relude.Extra (insertWith)

-- | Wrapping type for making an "In" FromJSON instance
--
-- @since 0.1.0.0
newtype In a = In a deriving (Show, Generic)

-- | Helper to override and provide default value when writing FromJSON instance
--
-- @since 0.1.0.0
wrappedParseJSON :: FromJSON a => String -> Text -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> (o .: key >>= (<?> Key key) . parseJSON)

-- | @since 0.1.0.0
filterKeysParseJSON ::
  -- | list of key that is allowed
  [Text] ->
  -- | original parseJSON
  (Value -> Parser a) ->
  (Value -> Parser a)
filterKeysParseJSON updatableKeys parser =
  withObject "update" $
    parser . Object . mapWithKey (\k -> if k `notElem` updatableKeys then const Null else id)

-- | The value is only inserted when the key is not present in the Object
--
-- @since 0.1.0.0
insert' ::
  -- | key
  Text ->
  -- | value
  Value ->
  -- | original Object
  Object ->
  Object
insert' = insertWith @Object (\_ old -> old)
