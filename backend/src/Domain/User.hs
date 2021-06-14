{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.User where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericParseJSON)
import Data.Generic.HKD (Construct (construct))
import Data.Generics.Labels ()
import Data.UUID (UUID)
import Domain.Util (Bio (..), Email (..), Image (..), In (..), Out, Password (..), Token, Username (..), wrappedParseJSON, wrappedToEncoding)
import GHC.TypeLits (Symbol)
import Validation.Adaptor (WithUpdate, WithValidation)

data family UserR (r :: Symbol)

newtype instance UserR "id" = UserId UUID

data instance UserR "all" = User
  { email :: Email, -- "jake@jake.jake",
    token :: Token, -- "jwt.token.here",
    password :: Password, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Maybe Bio, -- "I work at statefarm",
    image :: Maybe Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
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
    bio :: Maybe Bio, -- "I work at statefarm",
    image :: Maybe Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
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
    bio :: Maybe Bio, -- "I work at statefarm",
    image :: Maybe Image, -- "https://static.productionready.io/images/smiley-cyrus.jpg",
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
  parseJSON = wrappedParseJSON "user login" "user"

data instance UserR "register" = UserRegister
  { username :: Username,
    email :: Email,
    password :: Password
  }
  deriving (Generic, Show)

-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (UserR "register")) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Right (Failure ("null email" :| []))
instance FromJSON (WithValidation (UserR "register")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- >>> import Data.Aeson
-- >>> eitherDecode @(In (V.Validation (NonEmpty Text) (UserR "register"))) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Left "Error in $: key \"user\" not found"
instance FromJSON (In (WithValidation (UserR "register"))) where
  parseJSON = wrappedParseJSON "user register" "user"

data instance UserR "update" = UserUpdate
  { email :: Email, -- "jake@jake.jake",
    password :: Password, -- "jakejake"
    username :: Username, -- "jake",
    bio :: Bio, -- "I work at statefarm",
    image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
  }
  deriving (Generic, Show, Eq)

-- >>> import Data.Aeson
-- >>> eitherDecode @(WithUpdate (UserR "update")) "{\"email\": \"fjwofjoew\"}"
-- >>> eitherDecode @(WithUpdate (UserR "update")) "{\"email\": \"ohohhoh\",\"email\": \"fjwofjoew\",\"image\":null}"
-- >>> eitherDecode @(WithUpdate (UserR "update")) "{\"email\": \"fjwofjoew\"}"
-- >>> eitherDecode @(WithUpdate (UserR "update")) "{\"email\": \"ohohhoh\",\"email\": \"fjwofjoew\",\"image\":null}"
-- Right UserUpdate {email = Just (Last {getLast = Success "fjwofjoew"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}
-- Right UserUpdate {email = Just (Last {getLast = Success "ohohhoh"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}
-- Right UserUpdate {email = Just (Last {getLast = Success "fjwofjoew"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}
-- Right UserUpdate {email = Just (Last {getLast = Success "ohohhoh"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}
instance FromJSON (WithValidation (UserR "update")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- >>> import Data.Aeson
-- >>> import Data.Maybe (fromJust)
-- >>> import Data.Semigroup as SG
-- >>> b = UserUpdate (Email "hihi") (Password "pw") (Username "jack") (Bio "hi") (Image "jpg")
-- >>> c = eitherDecode @(HKD (HKD (HKD (UserR "update") WithValidation) SG.Last) Maybe) "{\"email\": \"fjwofjoew\"}"
-- >>> d = eitherDecode @(HKD (HKD (HKD (UserR "update") WithValidation) SG.Last) Maybe) "{\"email: \"fjwofjoew\"}"
-- >>> f = eitherDecode @(HKD (HKD (HKD (UserR "update") WithValidation) SG.Last) Maybe) "{\"email\": \"\"}"
-- >>> b
-- >>> c
-- >>> update c = fmap construct <<$>> (construct <<$>> construct . (deconstruct (deconstruct $ deconstruct b) <>) <$> c)
-- >>> SG.getLast . fromJust <$> update c
-- >>> update d
-- >>> update f
-- UserUpdate {email = "hihi", password = ********, username = "jack", bio = "hi", image = "jpg"}
-- Right UserUpdate {email = Just (Last {getLast = Success "fjwofjoew"}), password = Nothing, username = Nothing, bio = Nothing, image = Nothing}
-- Right (Success (UserUpdate {email = "fjwofjoew", password = ********, username = "jack", bio = "hi", image = "jpg"}))
-- Left "Error in $: Failed reading: satisfyWith. Expecting ':' at 'fjwofjoew}'"
-- Right (Just (Last {getLast = Failure ("null email" :| [])}))
instance FromJSON (WithUpdate (UserR "update"))

instance FromJSON (In (WithUpdate (UserR "update"))) where
  parseJSON = wrappedParseJSON "UserUpdate" "user"
