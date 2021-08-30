{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some Types appear in fields of Domain Types. Most of them are just newtype of 'Text'
--
-- @since 0.1.0.0
module Domain.Util.Field where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), withText)
import qualified Data.Text as T (null)
import Data.Time (UTCTime)
import Domain.Util.JSON.To (Out, wrappedToEncoding)
import Domain.Util.Validation (NoValidation (..), WithNoValidation, WithValidation, validate)
import Servant (FromHttpApiData)
import Text.Show (Show (showsPrec), showString)

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

-- | __FIXME__: Hashing of password in storage
--
-- @since 0.1.0.0
newtype Password = Password Text deriving newtype (Eq, ToJSON)

-- | __FIXME__: Refine validation of 'Password'
--
-- @since 0.1.0.0
instance FromJSON (WithValidation Password) where
  parseJSON = withText "password" $ pure <$> (Password <<$>> validate (not . T.null) "null password")

-- | Never show password
--
-- @since 0.1.0.0
instance Show Password where
  showsPrec _ _ = showString "********"

-- ** Username

-- | @since 0.1.0.0
newtype Username = Username Text
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Username)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Username)

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
  deriving newtype (Show, Eq, ToJSON)

-- | @since 0.1.0.0
instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure <$> (Title <<$>> validate (not . T.null) "null title")

-- ** Description

-- | @since 0.1.0.0
newtype Description = Description Text deriving newtype (Show, Eq, ToJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Description)

-- ** Body

-- | @since 0.1.0.0
newtype Body = Body Text deriving newtype (Show, Eq, ToJSON)

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
  toEncoding = wrappedToEncoding "tags"

-- * Other fields

-- ** Time

-- | @since 0.1.0.0
type Time = UTCTime
