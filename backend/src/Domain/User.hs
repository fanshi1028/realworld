{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.User where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value (Null, Object), defaultOptions, genericParseJSON, withObject)
import Data.Generic.HKD (Construct (construct), HKD (HKD))
import Data.Generics.Labels ()
import Data.HashMap.Strict (mapWithKey)
import Domain.Util.Field (Bio, Email, Image, Password, Token, Username)
import Domain.Util.JSON.From (In, updatableParseJSON, wrappedParseJSON)
import Domain.Util.JSON.To (Out, wrappedToEncoding)
import Domain.Util.Representation (Transform (transform), TransformM (transformM))
import GHC.TypeLits (Symbol)
import Relude.Extra (un)
import Validation.Carrier.Selective (WithUpdate, WithValidation)

data family UserR (r :: Symbol)

newtype instance UserR "id" = UserId Username
  deriving (Generic)
  deriving newtype (Show, Eq, FromJSON, Hashable)

instance FromJSON (WithValidation (UserR "id")) where
  parseJSON = fmap UserId <<$>> parseJSON

data instance UserR "all" = User
  { email :: Email, -- "jake@jake.jake",
    token :: Token, -- "jwt.token.here",
    password :: Password, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image, -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    following :: HashSet (UserR "id"), -- empty,
    followBy :: HashSet (UserR "id") -- empty,
  }
  deriving (Generic, Show, Eq)

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- Users (for authentication)
data instance UserR "auth" = UserAuth
  { email :: Email, -- "jake@jake.jake",
    token :: Token, -- "jwt.token.here",
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show, Eq, ToJSON)

-- >>> import Domain.Util
-- >>> import Data.Aeson
-- >>> user = UserAuth (Email "jake@jake.jake") (Token "jwt.token.here") (Username "jake") (Just $ Bio "I work at statefarm") (Just $ Image "https://static.productionready.io/images/smiley-cyrus.jpg")
-- >>> encode $ Out user
-- "{\"user\":{\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\",\"bio\":\"I work at statefarm\",\"email\":\"jake@jake.jake\",\"username\":\"jake\",\"token\":\"jwt.token.here\"}}"
instance ToJSON (Out (UserR "auth")) where
  toEncoding = wrappedToEncoding "user"

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
  transform User {..} = UserAuth email token username bio image

instance Transform UserR "auth" "id" where
  transform UserAuth {..} = UserId username

-- FIXME
instance TransformM m UserR "create" "all" where
  transformM = undefined

-- FIXME
instance TransformM m UserR "all" "profile" where
  transformM = undefined
