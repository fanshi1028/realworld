{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for user
--
-- @since 0.1.0.0
module User where

import Authentication (AuthOf)
import Data.Aeson (ToJSON (toEncoding, toJSON), Value (Object), defaultOptions, genericToJSON)
import qualified Data.HashMap.Strict as HM
import Domain (Domain (User))
import GHC.TypeLits (Symbol)
import Servant.Auth.Server (ToJWT (encodeJWT))
import Token (TokenOf)
import Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import Field.Email (Email(Email))
-- >>> import Field.Username (Username(Username))
-- >>> import Field.Bio (Bio(Bio))
-- >>> import Field.Image (Image(Image))
-- >>> import Token (TokenOf(UserToken))
-- >>> import Authentication (AuthOf(UserAuth))
-- >>> import Util.JSON.To (Out (Out))

-- | Type family for different representations of users
--
-- @since 0.1.0.0
data family UserR (r :: Symbol)

-- * profile

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
-- @since 0.2.0.0
data instance UserR "profile" = UserProfile
  { profile :: AuthOf 'User,
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
-- "{\"profile\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"}}"

-- | @since 0.2.0.0
instance ToJSON (Out (UserR "profile")) where
  toJSON = wrappedToJSON "profile"
  toEncoding = wrappedToEncoding "profile"

-- | Representation for auth info
--
-- @since 0.2.0.0
data instance UserR "authWithToken" = UserAuthWithToken (AuthOf 'User) (TokenOf 'User) deriving (Show, Eq, Generic)

-- | @since 0.2.0.0
instance ToJSON (UserR "authWithToken") where
  toJSON (UserAuthWithToken auth token) =
    case toJSON auth of
      Object hm -> Object $ HM.insert "token" (toJSON token) hm
      _ -> error "impossible in ToJSON (UserR \"authWithToken\")"

-- | @since 0.2.0.0
instance ToJWT (UserR "authWithToken") where
  encodeJWT (UserAuthWithToken auth _) = encodeJWT auth

-- | @since 0.2.0.0
instance ToJSON (Out (UserR "authWithToken")) where
  toJSON = wrappedToJSON "user"
  toEncoding = wrappedToEncoding "user"
-- ^
-- >>> encode $ Out $ UserAuthWithToken (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) (UserToken "jwt.token.here")
-- "{\"user\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"token\":\"jwt.token.here\"}}"
