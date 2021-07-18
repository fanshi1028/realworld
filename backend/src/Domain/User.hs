{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.User where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Object), defaultOptions, genericParseJSON, genericToJSON)
-- import Data.Generics.Labels ()

import Data.Aeson.Encoding (value)
import Data.ByteString.Base64.Type (ByteString64)
import Data.Generic.HKD (Construct (construct), HKD (HKD))
import qualified Data.HashMap.Strict as HM
import Domain.Util.Field (Bio, Email, Image, Password, Username)
import Domain.Util.JSON.From (In, updatableParseJSON, wrappedParseJSON)
import Domain.Util.JSON.To (Out (Out), wrapEncoding, wrappedToEncoding)
import Domain.Util.Representation (Transform (transform))
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData (parseUrlPiece))
import Servant.Auth.Server (FromJWT, ToJWT)
import Validation.Carrier.Selective (NoValidation, NoValidation' (NoValidation'), WithUpdate, WithValidation)

data family UserR (r :: Symbol)

newtype instance UserR "id" = UserId Username
  deriving (Generic)
  deriving newtype (Show, Eq, FromJSON, Hashable, ToJSON)

instance ToJWT (UserR "id")

instance FromJWT (UserR "id")

instance FromJSON (WithValidation (UserR "id")) where
  parseJSON = fmap UserId <<$>> parseJSON

newtype instance UserR "token" = Token ByteString64
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable, IsString)
  deriving (Generic)

instance FromJSON (WithValidation (UserR "token")) where
  parseJSON = pure <<$>> genericParseJSON defaultOptions

instance FromHttpApiData (UserR "token") where
  parseUrlPiece =
    ( >>=
        \case
          (words . show -> [prefix, token])
            | (prefix == "Token") -> pure $ Token $ show token
          _ -> Left "Authentication Header should be in format: \"Authorization: Token jwt.token.here\""
    )
      <$> parseUrlPiece @ByteString64

data instance UserR "all" = User
  { email :: Email, -- "jake@jake.jake",
  -- token :: UserR "token", -- "jwt.token.here",
    password :: Password, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image, -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    following :: HashSet (UserR "id"), -- empty,
    followBy :: HashSet (UserR "id") -- empty,
  }
  deriving (Generic, Show, Eq)

data instance UserR "auth" = UserAuth
  { email :: Email, -- "jake@jake.jake",
  -- token :: UserR "token", -- "jwt.token.here",
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show, Eq, ToJSON)

instance ToJWT (UserR "auth")

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- Users (for authentication)
data instance UserR "authWithToken" = UserAuthWithToken (UserR "auth") (UserR "token") deriving (Generic, Show, Eq)

-- FIXME Why is this instance needed for the instance Out instance below
instance ToJSON (UserR "authWithToken") where
  toEncoding (UserAuthWithToken auth token) =
    case genericToJSON defaultOptions auth of
      Object hm -> value $ Object $ HM.insert "token" (toJSON token) hm
      -- FIXME
      _ -> undefined -- impossible case

-- >>> import Domain.Util.Field
-- >>> import Data.Aeson
-- >>> user = UserAuthWithToken (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) (Token "jwt.token.here")
-- >>> encode $ Out user
instance ToJSON (Out (UserR "authWithToken")) where
  toEncoding (Out (UserAuthWithToken auth token)) = case genericToJSON defaultOptions auth of
    Object hm -> wrapEncoding "user" $ value $ Object $ HM.insert "token" (toJSON token) hm
    -- FIXME
    _ -> undefined -- impossible case

-- Profile
data instance UserR "profile" = UserProfile
  { email :: Email, -- "jake@jake.jake",
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image, -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    following :: Bool -- false
  }
  deriving (Generic, ToJSON)

-- >>> import Domain.Util
-- >>> import Data.Aeson
-- >>> profile = UserProfile (Email "jake@jake.jake") (Username "jake") (Just $ Bio "I work at statefarm") (Just $ Image "https://static.productionready.io/images/smiley-cyrus.jpg") False
-- >>> encode $ Out profile
-- "{\"profile\":{\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"bio\":\"I work at statefarm\",\"email\":\"jake@jake.jake\",\"following\":false,\"username\":\"jake\"}}"
instance ToJSON (Out (UserR "profile")) where
  toEncoding = wrappedToEncoding "profile"

-------------------
--   "           --
-- mmm    m mm   --
--   #    #"  #  --
--   #    #   #  --
-- mm#mm  #   #  --
-------------------

data instance UserR "login" = UserLogin
  { email :: Email,
    password :: Password
  }
  deriving (Generic, Show)

-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (UserR "login")) "{\"email\": \"ejfowfow@\", \"password\":\"11\" }"
-- Right (Success (UserLogin {email = "ejfowfow@", password = ********}))
instance FromJSON (WithValidation (UserR "login")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- >>> import Data.Aeson
-- >>> eitherDecode @(In (WithValidation (UserR "login"))) "{ \"user\": {\"email\": \"ejfowfow@\", \"password\":\"11\" } }"
-- Right (In (Success (UserLogin {email = "ejfowfow@", password = ********})))
instance FromJSON (In (WithValidation (UserR "login"))) where
  parseJSON = wrappedParseJSON "UserLogin" "user"

-- FIXME
instance FromHttpApiData (UserR "id") where
  parseUrlPiece = undefined

data instance UserR "create" = UserRegister
  { username :: Username,
    email :: Email,
    password :: Password
  }
  deriving (Generic, Show)

-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (UserR "create")) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Right (Failure ("null email" :| []))
instance FromJSON (WithValidation (UserR "create")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- >>> import Data.Aeson
-- >>> eitherDecode @(In (V.Validation ValidationErr (UserR "create"))) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Left "Error in $: key \"user\" not found"
instance FromJSON (In (WithValidation (UserR "create"))) where
  parseJSON = wrappedParseJSON "UserRegister" "user"

-- newtype instance UserR "update" = UserUpdate (UserR "all")
newtype instance UserR "update" = UserUpdate (WithUpdate (UserR "all"))
  -- { email :: Email, -- "jake@jake.jake",
  --   password :: Password, -- "jakejake"
  --   username :: Username, -- "jake",
  --   bio :: Bio, -- "I work at statefarm",
  --   image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  -- }
  deriving (Generic, Show, Eq)

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

instance FromJSON (UserR "update") where
  parseJSON =
    updatableParseJSON ["email", "password", "username", "bio", "image"] $
      genericParseJSON @(WithUpdate (UserR "all")) defaultOptions

instance FromJSON (In (UserR "update")) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"

---------------------------------------------------------------------
-- mmmmmmm                               m""                       --
--    #     m mm   mmm   m mm    mmm   mm#mm   mmm    m mm  mmmmm  --
--    #     #"  " "   #  #"  #  #   "    #    #" "#   #"  " # # #  --
--    #     #     m"""#  #   #   """m    #    #   #   #     # # #  --
--    #     #     "mm"#  #   #  "mmm"    #    "#m#"   #     # # #  --
---------------------------------------------------------------------

instance Transform UserR "all" "auth" where
  transform User {..} = UserAuth email username bio image

instance (HasField "username" (UserR s) Username) => Transform UserR s "id" where
  transform = UserId . getField @"username"
