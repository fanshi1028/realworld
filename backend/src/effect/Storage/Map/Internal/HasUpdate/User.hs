{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasUpdate.User where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import Data.Generic.HKD (HKD, build, construct)
import qualified Data.Semigroup as SG
import Domain (Domain (User))
import Field.Bio (Bio)
import Field.Email (Email)
import Field.Image (Image)
import Field.Password (Password)
import Field.Username (Username)
import Storage.Map.Internal.HasUpdate (HasUpdate (..), Patch, updatableKeys)
import Util.JSON.From (In, wrappedParseJSON)
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance HasUpdate 'User where
  data UpdateOf 'User = UserUpdate
    { email :: Email, -- "jake@jake.jake",
      password :: Password, -- "jakejake"
      username :: Username, -- "jake",
      bio :: Bio, -- "I work at statefarm",
      image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    }
    deriving (Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (Patch (UpdateOf 'User))) where
  parseJSON = withObject "UpdateUser" $ \o -> do
    updatableKeys ["email", "password", "username", "bio", "image"] o
    construct
      <$> construct
        ( build @(HKD (HKD (HKD (UpdateOf 'User) SG.Last) Maybe) WithValidation)
            (o .:? "email" .!= pure Nothing)
            (o .:? "password" .!= pure Nothing)
            (o .:? "username" .!= pure Nothing)
            (o .:? "bio" .!= pure Nothing)
            (o .:? "image" .!= pure Nothing)
        )

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (Patch (UpdateOf 'User)))) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"
