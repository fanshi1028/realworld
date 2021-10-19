{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Utils to tweak FromJSON instances
--
-- @since 0.1.0.0
module Util.JSON.From
  ( -- * Types
    In (..),

    -- * Helpers
    insert',
    wrappedParseJSON,
  )
where

import Data.Aeson (FromJSON, Value, withObject, (.:))
import Data.Aeson.Types (Object, Parser)
import Relude.Extra (insertWith)

-- | Wrapping type for making an "In" FromJSON instance
--
-- @since 0.1.0.0
newtype In a = In a deriving (Show, Generic)

-- | Helper to override and provide default value when writing FromJSON instance
--
-- @since 0.1.0.0
wrappedParseJSON :: FromJSON a => String -> Text -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> o .: key

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
insert' = insertWith (\_ old -> old)
