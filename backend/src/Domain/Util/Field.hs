{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some Types appear in fields of Domain Types. Most of them are just newtype of 'Text'
--
-- @since 0.1.0.0
module Domain.Util.Field where

import Data.Password.Argon2 (mkPassword)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), withText)
import qualified Data.Password.Argon2 as Argon2 (Argon2, Password, PasswordHash)
import qualified Data.Text as T (intercalate, null, toLower)
import Data.Time (UTCTime)
import Domain.Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)
import Domain.Util.Validation (NoValidation (..), WithNoValidation, WithValidation, validate)
import Servant (FromHttpApiData)

-- * Text-like fields

-- ** Email

-- | @since 0.1.0.0
newtype Email = Email Text deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON)

-- | __FIXME__: Refine validation of 'Email'
--
-- @since 0.1.0.0
instance FromJSON (WithValidation Email) where
  parseJSON = withText "email" $ pure <$> (Email <<$>> validate (not . T.null) "null email")

-- ** Password

-- | Password hashed using Argon2
--
-- @since 0.2.0.0
type PasswordHash = Argon2.PasswordHash Argon2.Argon2

-- | @since 0.2.0.0
instance FromJSON Argon2.Password where
  parseJSON = mkPassword <<$>> parseJSON

-- ** Username

-- | @since 0.1.0.0
newtype Username = Username Text
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Username)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Username)

-- ** Bio

-- | @since 0.1.0.0
newtype Bio = Bio Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Bio)

-- ** Image

-- | @since 0.1.0.0
newtype Image
  = -- | 'Text' is the link of the image
    Image Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Image)

-- ** Slug

-- | @since 0.1.0.0
newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON, Hashable)

-- | __FIXME__: Slug need validation?
--
--  @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Slug)

-- | __FIXME__: Slug need validation?
--
--  @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Slug)

-- ** Title

-- | @since 0.1.0.0
newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure <$> (Title <<$>> validate (not . T.null) "null title")

-- ** Description

-- | @since 0.1.0.0
newtype Description = Description Text deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Description)

-- ** Body

-- | @since 0.1.0.0
newtype Body = Body Text deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Body)

-- ** Tag

-- | @since 0.1.0.0
newtype Tag = Tag Text deriving newtype (Show, Eq, Hashable, ToJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Tag)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Tag)

-- | @since 0.1.0.0
instance (Foldable t, ToJSON (t Tag)) => ToJSON (Out (t Tag)) where
  toJSON = wrappedToJSON "tags"
  toEncoding = wrappedToEncoding "tags"

-- * Other fields

-- ** Time

-- | @since 0.1.0.0
type Time = UTCTime

-- * Some helper functions

-- | @since 0.2.0.0
titleToSlug :: Title -> Slug
titleToSlug (Title t) = Slug $ T.intercalate "-" $ words $ T.toLower t
