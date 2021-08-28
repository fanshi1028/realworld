{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module Domain.Util.JSON.From (In (..), insert', wrappedParseJSON, filterKeysParseJSON) where

import Data.Aeson (FromJSON (parseJSON), Value (Null, Object), withObject, (.:), (<?>))
import Data.Aeson.Types (JSONPathElement (Key), Object, Parser)
import Data.HashMap.Strict (mapWithKey)
import Relude.Extra (insertWith)

-- | helper to override and provide default value when writing FromJSON instance
insert' :: Text -> Value -> Object -> Object
insert' = insertWith @Object (\_ x -> x)

-- | wrapping type to make an "in" JSON representation
newtype In a = In a deriving (Show, Generic)

wrappedParseJSON :: FromJSON a => String -> Text -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> (o .: key >>= (<?> Key key) . parseJSON)

filterKeysParseJSON ::
  [Text] ->
  (Value -> Parser a) ->
  (Value -> Parser a)
filterKeysParseJSON updatableKeys parser =
  withObject
    "update"
    $ parser
      . Object
      . mapWithKey (\k -> if k `notElem` updatableKeys then const Null else id)
