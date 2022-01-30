{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Tag
--
-- @since 0.4.0.0
module Data.Field.Tag where

import Data.Aeson (FromJSON, ToJSON (toEncoding), toJSON)
import Data.Text (split, strip)
import qualified Data.Text as T (intercalate)
import Data.Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import Servant (FromHttpApiData)

-- | @since 0.2.0.0
newtype Tag = Tag
  { -- | @since 0.4.0.0
    unTag :: Text
  }
  deriving newtype (Show, Eq, Hashable, ToJSON)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Tag)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Tag)

-- | @since 0.2.0.0
instance (Foldable t, ToJSON (t Tag)) => ToJSON (Out (t Tag)) where
  toJSON = wrappedToJSON "tags"
  toEncoding = wrappedToEncoding "tags"

-- | @since 0.4.0.0
-- >>> splitToTags "hi,yo,bye"
-- ["hi","yo","bye"]
splitToTags :: Text -> [Tag]
splitToTags = (Tag . strip <$>) . filter (/= "") . split (== ',')

-- | @since 0.4.0.0
-- >>> tagsToText [ Tag "hi", Tag "yo", Tag "bye" ]
-- "hi,yo,bye"
tagsToText :: [Tag] -> Text
tagsToText = T.intercalate "," . fmap unTag
