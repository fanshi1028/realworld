{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Authentication.Internal.HasAuth.User where

import Authentication.Internal.HasAuth (HasAuth (..))
import Data.Aeson (FromJSON (parseJSON), ToJSON, defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Domain (Domain (User))
import Field.Bio (Bio)
import Field.Email (Email)
import Field.Image (Image)
import Field.Password (Password)
import Field.Username (Username)
import Servant.Auth.Server (FromJWT, ToJWT)
import Util.JSON.From (In, wrappedParseJSON)
import Util.Validation (WithValidation)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
instance HasAuth 'User where
  data LoginOf 'User = UserLogin
    { email :: Email,
      password :: Password
    }
    deriving (Show, Generic)
  data AuthOf 'User = UserAuth
    { email :: Email, -- "jake@jake.jake",
    -- token :: UserR "token", -- "jwt.token.here",
      username :: Username, -- "jake",
      bio :: Bio, -- "I work at statefarm",
      image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    }
    deriving (Show, Eq, Generic)

-- | @since 0.2.0.0
instance FromJSON (LoginOf 'User)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (LoginOf 'User)) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- >>> eitherDecode' @(WithValidation (LoginOf 'User)) "{\"email\": \"ejfowfow@\", \"password\":\"11832hf92hf2389f\" }"
-- Right (Success (UserLogin {email = "ejfowfow@", password = **PASSWORD**}))

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (LoginOf 'User))) where
  parseJSON = wrappedParseJSON "UserLogin" "user"
-- ^
-- >>> eitherDecode' @(In (WithValidation (LoginOf 'User))) "{ \"user\": {\"email\": \"ejfowfow@\", \"password\":\"11239h2389f9328\" } }"
-- Right (In (Success (UserLogin {email = "ejfowfow@", password = **PASSWORD**})))

-- | @since 0.2.0.0
instance FromJSON (AuthOf 'User)

-- | @since 0.2.0.0
instance ToJSON (AuthOf 'User)

-- | @since 0.2.0.0
instance FromJWT (AuthOf 'User)

-- | @since 0.2.0.0
instance ToJWT (AuthOf 'User)
