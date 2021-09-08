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

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Object), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Encoding (value)
import Data.Generic.HKD (Construct (construct))
import qualified Data.HashMap.Strict as HM (insert)
import Data.Password.Argon2 (Password)
import Domain.Util.Field (Bio, Email, Image, PasswordHash, Username)
import Domain.Util.JSON.From (In, filterKeysParseJSON, wrappedParseJSON)
import Domain.Util.JSON.To (Out (Out), wrapEncoding)
import Domain.Util.Update (WithUpdate)
import Domain.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData (parseUrlPiece))
import Servant.Auth.Server (FromJWT, ToJWT (encodeJWT))

-- | Type family for different representations of users
--
-- @since 0.1.0.0
data family UserR (r :: Symbol)

-- | Id which can be used to uniquely idenitify an user.
--
-- @since 0.1.0.0
newtype instance UserR "id" = UserId Username
  deriving (Generic)
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
data instance UserR "authWithToken" = UserAuthWithToken (UserR "auth") (UserR "token") deriving (Generic, Show, Eq)

-- | @since 0.1.0.0
instance ToJSON (UserR "authWithToken") where
  toEncoding (UserAuthWithToken auth token) =
    case genericToJSON defaultOptions auth of
      Object hm -> value $ Object $ HM.insert "token" (toJSON token) hm
      _ -> error "impossible in ToJSON (UserR \"authWithToken\")"

-- | @since 0.1.0.0
instance ToJWT (UserR "authWithToken") where
  encodeJWT (UserAuthWithToken auth _) = encodeJWT auth

-- |
-- >>> import Domain.Util.Field
-- >>> import Data.Aeson
-- >>> user = UserAuthWithToken (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) (Token "jwt.token.here")
-- >>> encode $ Out user
--
-- @since 0.1.0.0
instance ToJSON (Out (UserR "authWithToken")) where
  toEncoding (Out a) = wrapEncoding "user" $ toEncoding a

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
  deriving (Generic, ToJSON)

-- |
-- >>> import Domain.Util.Field
-- >>> import Data.Aeson
-- >>> profile = UserProfile (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) False
-- >>> encode $ Out profile
-- "{\"profile\":{\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"bio\":\"I work at statefarm\",\"email\":\"jake@jake.jake\",\"following\":false,\"username\":\"jake\"}}"
--
-- @since 0.1.0.0
instance ToJSON (Out (UserR "profile")) where
  toEncoding (Out (UserProfile auth following')) = wrapEncoding "profile" $
    case genericToJSON defaultOptions auth of
      Object hm -> value $ Object $ HM.insert "following" (toJSON following') hm
      _ -> error "impossible in ToJSON (UserR \"profile\")"

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
  deriving (Generic, Show)

-- |
-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (UserR "login")) "{\"email\": \"ejfowfow@\", \"password\":\"11\" }"
-- Right (Success (UserLogin {email = "ejfowfow@", password = ********}))
--
-- @since 0.1.0.0
instance FromJSON (WithValidation (UserR "login")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- |
-- >>> import Data.Aeson
-- >>> eitherDecode @(In (WithValidation (UserR "login"))) "{ \"user\": {\"email\": \"ejfowfow@\", \"password\":\"11\" } }"
-- Right (In (Success (UserLogin {email = "ejfowfow@", password = ********})))
--
-- @since 0.1.0.0
instance FromJSON (In (WithValidation (UserR "login"))) where
  parseJSON = wrappedParseJSON "UserLogin" "user"

-- | Representation for creation
--
-- @since 0.1.0.0
data instance UserR "create" = UserRegister
  { username :: Username,
    email :: Email,
    password :: Password
  }
  deriving (Generic, Show)

-- |
-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (UserR "create")) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Right (Failure ("null email" :| []))
--
-- @since 0.1.0.0
instance FromJSON (WithValidation (UserR "create")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- |
-- >>> import Data.Aeson
-- >>> eitherDecode @(In (V.Validation ValidationErr (UserR "create"))) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Left "Error in $: key \"user\" not found"
--
-- @since 0.1.0.0
instance FromJSON (In (WithValidation (UserR "create"))) where
  parseJSON = wrappedParseJSON "UserRegister" "user"

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
newtype instance UserR "update" = UserUpdate (WithUpdate (UserR "updateInternal"))
  -- newtype instance UserR "update" = UserUpdate (WithUpdate (UserR "all"))
  -- { email :: Email, -- "jake@jake.jake",
  --   password :: Password, -- "jakejake"
  --   username :: Username, -- "jake",
  --   bio :: Bio, -- "I work at statefarm",
  --   image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  -- }
  deriving (Generic)

-- >>> import Data.Aeson
-- >>> eitherDecode @(UserR "update") "{\"email\": \"fjwofjoew\"}"
-- >>> eitherDecode @(UserR "update") "{\"email\": \"ohohhoh\",\"email\": \"fjwofjoew\",\"image\":null}"
-- >>> eitherDecode @(UserR "update") "{\"email\": \"fjwofjoew\", \"token\": \"hi\"}"
-- >>> eitherDecode @(UserR "update") "{\"email\": \"ohohhoh\",\"email\": \"fjwofjoew\",\"image\":null}"
-- Right (UserUpdate User {email = Just (Last {getLast = Success "fjwofjoew"}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})
-- Right (UserUpdate User {email = Just (Last {getLast = Success "ohohhoh"}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})
-- Right (UserUpdate User {email = Just (Last {getLast = Success "fjwofjoew"}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})
-- Right (UserUpdate User {email = Just (Last {getLast = Success "ohohhoh"}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})

-- >>> import Data.Aeson
-- >>> import Data.Maybe (fromJust)
-- >>> import Data.Semigroup as SG
-- >>> import Domain.Util.Field
-- >>> -- b = UserUpdate (Email "hihi") (Password "pw") (Username "jack") (Bio "hi") (Image "jpg")
-- >>> -- b
-- >>> -- update c = fmap construct <<$>> (construct <<$>> construct . (deconstruct (deconstruct $ deconstruct b) <>) <$> c)
-- >>> -- SG.getLast . fromJust <$> update c
-- >>> -- update d
-- >>> -- update f
-- >>> c = eitherDecode @(UserR "update") "{\"email\": \"fjwofjoew\"}"
-- >>> d = eitherDecode @(UserR "update") "{\"email: \"fjwofjoew\"}"
-- >>> f = eitherDecode @(UserR "update") "{\"email\": \"\"}"
-- >>> c
-- >>> d
-- >>> f
-- Right (UserUpdate User {email = Just (Last {getLast = Success "fjwofjoew"}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})
-- Left "Error in $: Failed reading: satisfyWith. Expecting ':' at 'fjwofjoew}'"
-- Right (UserUpdate User {email = Just (Last {getLast = Failure ("null email" :| [])}), token = Nothing, password = Nothing, username = Nothing, bio = Nothing, image = Nothing, following = Nothing, followBy = Nothing})

-- | @since 0.2.0.0
instance FromJSON (WithValidation (UserR "update")) where
  parseJSON =
    filterKeysParseJSON
      ["email", "password", "username", "bio", "image"]
      (fmap UserUpdate . construct <<$>> genericParseJSON defaultOptions)

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (UserR "update"))) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"
