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
-- @since 0.2.0.0
module Domain.User where

import Authentication.Internal.HasAuth (AuthOf)
import Data.Aeson (ToJSON (toEncoding, toJSON), Value (Object), defaultOptions, genericToJSON)
import qualified Data.HashMap.Strict as HM (insert)
import Domain (Domain (User))
import Servant.Auth.Server (ToJWT (encodeJWT))
import Token.Internal.HasToken (TokenOf)
import Token.Internal.HasToken.User ()
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
-- >>> exampleAuth = UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")
-- >>> exampleProfile = UserProfile exampleAuth False
-- >>> exampleAuthWithToken = UserAuthWithToken exampleAuth $ UserToken "jwt.token.here"

-- * profile

-- | @since 0.4.0.0
-- >>> exampleProfile
-- UserProfile {profile = UserAuth {email = "jake@jake.jake", username = "jake", bio = "I work at statefarm", image = "https://static.productionready.io/images/smiley-cyrus.jpg"}, following = False}
data UserProfile = UserProfile
  { profile :: AuthOf 'User,
    following :: Bool
  }
  deriving (Show, Eq, Generic)

-- | @since 0.4.0.0
instance ToJSON UserProfile where
  toJSON (UserProfile auth following') = case genericToJSON defaultOptions auth of
    Object hm -> Object $ HM.insert "following" (toJSON following') hm
    _ -> error "Impossible: in ToJSON UserProfile"
-- ^
-- >>> encode exampleProfile
-- "{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"}"

-- | @since 0.4.0.0
instance ToJSON (Out UserProfile) where
  toJSON = wrappedToJSON "profile"
  toEncoding = wrappedToEncoding "profile"
-- ^
-- >>> encode $ Out exampleProfile
-- "{\"profile\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"}}"

-- | @since 0.4.0.0
-- >>> exampleAuthWithToken
-- UserAuthWithToken (UserAuth {email = "jake@jake.jake", username = "jake", bio = "I work at statefarm", image = "https://static.productionready.io/images/smiley-cyrus.jpg"}) (UserToken "jwt.token.here")
data UserAuthWithToken = UserAuthWithToken (AuthOf 'User) (TokenOf 'User) deriving (Show, Eq, Generic)

-- | @since 0.4.0.0
instance ToJSON UserAuthWithToken where
  toJSON (UserAuthWithToken auth token) =
    case toJSON auth of
      Object hm -> Object $ HM.insert "token" (toJSON token) hm
      _ -> error "impossible in ToJSON UserAuthWithToken"
-- ^
-- >>> encode exampleAuthWithToken
-- "{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"token\":\"jwt.token.here\"}"

-- | @since 0.4.0.0
instance ToJWT UserAuthWithToken where
  encodeJWT (UserAuthWithToken auth _) = encodeJWT auth
-- ^
-- >>> encodeJWT exampleAuthWithToken
-- ClaimsSet {_claimIss = Nothing, _claimSub = Nothing, _claimAud = Nothing, _claimExp = Nothing, _claimNbf = Nothing, _claimIat = Nothing, _claimJti = Nothing, _unregisteredClaims = fromList [("dat",Object (fromList [("bio",String "I work at statefarm"),("email",String "jake@jake.jake"),("image",String "https://static.productionready.io/images/smiley-cyrus.jpg"),("username",String "jake")]))]}

-- | @since 0.4.0.0
instance ToJSON (Out UserAuthWithToken) where
  toJSON = wrappedToJSON "user"
  toEncoding = wrappedToEncoding "user"
-- ^
-- >>> encode $ Out exampleAuthWithToken
-- "{\"user\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"token\":\"jwt.token.here\"}}"
