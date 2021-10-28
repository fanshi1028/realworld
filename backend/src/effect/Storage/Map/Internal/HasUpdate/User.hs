{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Updating in storage for 'User'
--
-- @since 0.2.0.0
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

-- $setup
-- >>> import Data.Aeson (eitherDecode')

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
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation (Patch (UpdateOf 'User))) "{\"email\":\"ewofjowejf@gmai.com\"}"
-- Right (Success UserUpdate {email = Just (Last {getLast = "ewofjowejf@gmai.com"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing})
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (Patch (UpdateOf 'User))) "{\"notUpdatable\":\"hi\"}"
-- Left "Error in $: Only keys [\"email\",\"password\",\"username\",\"bio\",\"image\"] are updatable, while we found other keys: [\"notUpdatable\"]"

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (Patch (UpdateOf 'User)))) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'User)))) "{\"user\":{\"email\":\"ewofjowejf@gmai.com\"}}"
-- Right (In (Success UserUpdate {email = Just (Last {getLast = "ewofjowejf@gmai.com"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'User)))) "{\"email\":\"ewofjowejf@gmai.com\"}"
-- Left "Error in $: key \"user\" not found"
--
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'User)))) "{\"user\":{\"notUpdatable\":\"hi\"}}"
-- Left "Error in $.user: Only keys [\"email\",\"password\",\"username\",\"bio\",\"image\"] are updatable, while we found other keys: [\"notUpdatable\"]"
