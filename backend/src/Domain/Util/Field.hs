{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
module Domain.Util.Field
  ( Email (..),
    Password (..),
    Tag (..),
    Title (..),
    Slug (..),
    Username (..),
    Image (..),
    Bio (..),
    Body (..),
    Description (..),
    Time,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), withText)
import qualified Data.Text as T (null)
import Data.Time (UTCTime)
import Domain.Util.JSON.To (Out, wrappedToEncoding)
import Domain.Util.Validation (NoValidation, NoValidation' (..), WithValidation, validate)
import Servant (FromHttpApiData)
import Text.Show (Show (showsPrec), showString)

-- | some newtype

-- | Email
newtype Email = Email Text deriving newtype (Show, Eq, ToJSON, FromJSON)

instance FromJSON (WithValidation Email) where
  parseJSON = withText "email" $ pure <$> (Email <<$>> validate (not . T.null) "null email")

-- | Password
newtype Password = Password Text deriving newtype (Eq, ToJSON)

instance FromJSON (WithValidation Password) where
  parseJSON = withText "password" $ pure <$> (Password <<$>> validate (not . T.null) "null password")

instance Show Password where
  showsPrec _ _ = showString "********"

-- | Username
newtype Username = Username Text
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable)

deriving via (NoValidation Text) instance FromJSON (WithValidation Username)

deriving via (NoValidation Text) instance FromHttpApiData (WithValidation Username)

-- | Bio
newtype Bio = Bio Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- deriving (Generic)

deriving via (NoValidation Text) instance FromJSON (WithValidation Bio)

-- | Image
newtype Image = Image Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

deriving via (NoValidation Text) instance FromJSON (WithValidation Image)

-- | Slug
newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON, Hashable)

-- FIXME: Slug need validation?
deriving via (NoValidation Text) instance FromJSON (WithValidation Slug)

deriving via (NoValidation Text) instance FromHttpApiData (WithValidation Slug)

-- | Title
newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON)

instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure <$> (Title <<$>> validate (not . T.null) "null title")

-- | Description
newtype Description = Description Text deriving newtype (Show, Eq, ToJSON)

deriving via (NoValidation Text) instance FromJSON (WithValidation Description)

-- | Body
newtype Body = Body Text deriving newtype (Show, Eq, ToJSON)

deriving via (NoValidation Text) instance FromJSON (WithValidation Body)

-- | Tag
newtype Tag = Tag Text deriving newtype (Show, Eq, Hashable, ToJSON)

deriving via (NoValidation Text) instance FromJSON (WithValidation Tag)

deriving via (NoValidation Text) instance FromHttpApiData (WithValidation Tag)

instance (Foldable t, ToJSON (t Tag)) => ToJSON (Out (t Tag)) where
  toEncoding = wrappedToEncoding "tags"

-- | Time
type Time = UTCTime
