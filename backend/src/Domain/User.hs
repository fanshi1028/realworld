{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for user
--
-- @since 0.1.0.0
module Domain.User (UserR (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Object), defaultOptions, genericParseJSON, genericToJSON, withObject, (.!=), (.:?))
import Data.Generic.HKD (Construct (construct), HKD, build)
import qualified Data.HashMap.Strict as HM (insert)
import Data.Password.Argon2 (Password)
import qualified Data.Semigroup as SG (Last)
import Domain.Util.Field (Bio, Email, Image, PasswordHash, Username)
import Domain.Util.JSON.From (In, wrappedParseJSON)
import Domain.Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)
import Domain.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData (parseUrlPiece))
import Servant.Auth.Server (FromJWT, ToJWT (encodeJWT))

-- $setup
-- >>> import Data.Aeson (eitherDecode', encode)
-- >>> import Domain.Util.Field
-- >>> import Domain.Util.JSON.To (Out (Out))

-- | Type family for different representations of users
--
-- @since 0.1.0.0
data family UserR (r :: Symbol)

-- | Id which can be used to uniquely idenitify an user.
--
-- @since 0.1.0.0
newtype instance UserR "id" = UserId Username
  deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON)

-- | @since 0.1.0.0
deriving via (WithValidation Username) instance FromJSON (WithValidation (UserR "id"))

-- | @since 0.1.0.0
deriving via (WithValidation Username) instance FromHttpApiData (WithValidation (UserR "id"))

-- | Token which can be used to uniquely idenitify an user.
--
-- @since 0.1.0.0
newtype instance UserR "token" = UserToken Text
  deriving newtype (Show, Eq, ToJSON, Hashable)
  deriving (Generic)

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation (UserR "token"))

-- | @since 0.1.0.0
instance FromHttpApiData (UserR "token") where
  parseUrlPiece =
    ( >>=
        \case
          (words -> [prefix, token])
            | (prefix == "Token") -> pure $ UserToken token
          _ -> Left "Authentication Header should be in format: \"Authorization: Token jwt.token.here\""
    )
      <$> parseUrlPiece @Text

-- | Representation in storage
--
-- @since 0.1.0.0
data instance UserR "all" = User
  { email :: Email, -- "jake@jake.jake",
  -- token :: UserR "token", -- "jwt.token.here",
    password :: PasswordHash, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show, Eq)

-- | Sub-representation of user that is used to generate token
--
-- @since 0.1.0.0
data instance UserR "auth" = UserAuth
  { email :: Email, -- "jake@jake.jake",
  -- token :: UserR "token", -- "jwt.token.here",
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | @since 0.1.0.0
instance ToJWT (UserR "auth")

-- | @since 0.1.0.0
instance FromJWT (UserR "auth")

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- | Representation for auth info
--
-- @since 0.1.0.0
data instance UserR "authWithToken" = UserAuthWithToken (UserR "auth") (UserR "token") deriving (Show, Eq, Generic)

-- | @since 0.1.0.0
instance ToJSON (UserR "authWithToken") where
  toJSON (UserAuthWithToken auth token) =
    case toJSON auth of
      Object hm -> Object $ HM.insert "token" (toJSON token) hm
      _ -> error "impossible in ToJSON (UserR \"authWithToken\")"

-- | @since 0.1.0.0
instance ToJWT (UserR "authWithToken") where
  encodeJWT (UserAuthWithToken auth _) = encodeJWT auth

-- | @since 0.2.0.0
instance ToJSON (Out (UserR "authWithToken")) where
  toJSON = wrappedToJSON "user"
  toEncoding = wrappedToEncoding "user"
-- ^
-- >>> encode $ Out $ UserAuthWithToken (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) (UserToken "jwt.token.here")
-- "{\"user\":{\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"bio\":\"I work at statefarm\",\"email\":\"jake@jake.jake\",\"username\":\"jake\",\"token\":\"jwt.token.here\"}}"

-- Profile
-- data instance UserR "profile" = UserProfile
--   { email :: Email, -- "jake@jake.jake",
--     username :: Username, -- "jake",
--     bio :: Bio, -- "I work at statefarm",
--     image :: Image, -- "https://static.productionready.io/images/smiley-cyrus.jpg",
--     following :: Bool -- false
--   }
--   deriving (Generic, ToJSON)

-- | Representation for profile
--
-- @since 0.1.0.0
data instance UserR "profile" = UserProfile
  { profile :: UserR "auth",
    following :: Bool
  }
  deriving (Show, Eq, Generic)

-- | @since 0.2.0.0
instance ToJSON (UserR "profile") where
  toJSON (UserProfile auth following') = case genericToJSON defaultOptions auth of
    Object hm -> Object $ HM.insert "following" (toJSON following') hm
    _ -> error "impossible in ToJSON (UserR \"profile\")"
-- ^
-- >>> encode $ Out $ UserProfile (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) False
-- "{\"profile\":{\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"bio\":\"I work at statefarm\",\"email\":\"jake@jake.jake\",\"following\":false,\"username\":\"jake\"}}"

-- | @since 0.2.0.0
instance ToJSON (Out (UserR "profile")) where
  toJSON = wrappedToJSON "profile"
  toEncoding = wrappedToEncoding "profile"

-------------------
--   "           --
-- mmm    m mm   --
--   #    #"  #  --
--   #    #   #  --
-- mm#mm  #   #  --
-------------------

-- | Representation for login
--
-- @since 0.1.0.0
data instance UserR "login" = UserLogin
  { email :: Email,
    password :: Password
  }
  deriving (Show, Generic)

-- | @since 0.1.0.0
instance FromJSON (WithValidation (UserR "login")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- >>> eitherDecode' @(WithValidation (UserR "login")) "{\"email\": \"ejfowfow@\", \"password\":\"11832hf92hf2389f\" }"
-- Right (Success (UserLogin {email = "ejfowfow@", password = **PASSWORD**}))

-- | @since 0.1.0.0
instance FromJSON (In (WithValidation (UserR "login"))) where
  parseJSON = wrappedParseJSON "UserLogin" "user"
-- ^
-- >>> eitherDecode' @(In (WithValidation (UserR "login"))) "{ \"user\": {\"email\": \"ejfowfow@\", \"password\":\"11239h2389f9328\" } }"
-- Right (In (Success (UserLogin {email = "ejfowfow@", password = ********})))

-- | Representation for creation
--
-- @since 0.1.0.0
data instance UserR "create" = UserRegister
  { username :: Username,
    email :: Email,
    password :: Password
  }
  deriving (Show, Generic)

-- | @since 0.1.0.0
instance FromJSON (WithValidation (UserR "create")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- >>> eitherDecode' @(WithValidation (UserR "create")) "{\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\", \"password\":\"11fewifwofwwefew\" }"
-- Right (Success (UserRegister {username = "", email = "ff2239fj3902@fiew.mail", password = **PASSWORD**}))

-- | @since 0.1.0.0
instance FromJSON (In (WithValidation (UserR "create"))) where
  parseJSON = wrappedParseJSON "UserRegister" "user"
-- ^
-- >>> eitherDecode' @(In (WithValidation (UserR "create"))) "{ \"user\": {\"username\": \"\", \"email\": \"\", \"password\":\"11\" } }"
-- Right (In (Failure ("null email" :| ["PasswordTooShort 8 2"])))

-- | Since we need IO to hash the password,
-- we need an "updateInternal" representation as middleman
-- instead of just reusing the "all" representation
--
-- @since 0.2.0.0
data instance UserR "updateInternal" = UserUpdateInternal
  { email :: Email, -- "jake@jake.jake",
    password :: Password, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show)

-- | Representation for update
--
-- @since 0.1.0.0
newtype instance UserR "update" = UserUpdate (HKD (HKD (UserR "updateInternal") SG.Last) Maybe) deriving (Show, Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (UserR "update")) where
  parseJSON = withObject "UpdateUser" $ \o ->
    UserUpdate
      <<$>> construct
        <$> construct
          ( build @(HKD (HKD (HKD (UserR "updateInternal") SG.Last) Maybe) WithValidation)
              (o .:? "email" .!= pure Nothing)
              (o .:? "password" .!= pure Nothing)
              (o .:? "username" .!= pure Nothing)
              (o .:? "bio" .!= pure Nothing)
              (o .:? "image" .!= pure Nothing)
          )

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (UserR "update"))) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"
