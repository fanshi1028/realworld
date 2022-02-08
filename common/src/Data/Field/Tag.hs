{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), toJSON, withText)
import Data.Text (split, strip)
import qualified Data.Text as T (intercalate, null)
import Data.Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)
import Data.Util.Validation (WithValidation, validate)
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (parseQueryParam))
import Validation (Validation (Failure, Success))

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
newtype Tag = Tag
  { -- | @since 0.4.0.0
    unTag :: Text
  }
  deriving newtype (Show, Eq, Hashable, ToJSON)

-- | @since 0.4.0.0
instance FromJSON (WithValidation Tag) where
  parseJSON = withText "tag" $ pure . (Tag <<$>> validate (not . T.null) "null tag")
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation Tag) "\"jfowjfw@mmm\""
-- Right (Success "jfowjfw@mmm")
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Tag) "\"\""
-- Right (Failure ("null tag" :| []))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Tag) "{}"
-- Left "Error in $: parsing tag failed, expected String, but encountered Object"

-- | @since 0.4.0.0
instance FromHttpApiData (WithValidation Tag) where
  parseQueryParam inp =
    parseQueryParam inp
      <&> validate (not . T.null) "null tag"
      >>= \case
        Failure (e :| es) -> Left $ foldl' (\x y -> x <> "," <> y) e es
        Success t -> Right . Success $ Tag t
-- ^
-- ==== Success
-- >>> parseQueryParam @(WithValidation Tag) "jfowjfw@mmm"
-- Right (Success "jfowjfw@mmm")
--
-- ==== Validation Fail
-- >>> parseQueryParam @(WithValidation Tag) ""
-- Left "null tag"

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
