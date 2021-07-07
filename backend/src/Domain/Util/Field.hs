{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
module Domain.Util.Field where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), withText)
import qualified Data.Text as T (null)
import Data.Time (UTCTime)
import Domain.Util.JSON.To (Out, wrappedToEncoding)
import Text.Show (Show (showsPrec), showString)
import Validation.Carrier.Selective (NoValidation, NoValidation' (..), WithValidation, validate)
import Servant (FromHttpApiData)

-- | some newtype

-- | Email
newtype Email = Email Text deriving newtype (Show, Eq, ToJSON)

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable, FromHttpApiData)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Username)

-- | Token
newtype Token = Token Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Token)

-- | Bio
newtype Bio = Bio Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Bio)

-- | Image
newtype Image = Image Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Image)

newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Slug)

newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving via NoValidation instance FromJSON (WithValidation Title)

instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure <$> (Title <<$>> validate (not . T.null) "null title")

newtype Description = Description Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Description)

newtype Body = Body Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Body)

newtype Tag = Tag Text deriving newtype (Show, Eq, ToJSON, FromHttpApiData)

deriving via NoValidation instance FromJSON (WithValidation Tag)

instance (Foldable t, ToJSON (t Tag)) => ToJSON (Out (t Tag)) where
  toEncoding = wrappedToEncoding "tags"

type Time = UTCTime
