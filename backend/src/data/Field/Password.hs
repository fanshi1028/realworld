{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Password
--
-- @since 0.2.0.0
module Field.Password (Password (Password), PasswordHash (PasswordHash), hashPassword, checkPassword, newSalt) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Password.Argon2 (Argon2, PasswordCheck, defaultParams, hashPasswordWithSalt, mkPassword)
import qualified Data.Password.Argon2 as Argon2 (Password, PasswordHash, Salt, checkPassword, newSalt)
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance FromJSON Argon2.Password where
  parseJSON = mkPassword <<$>> parseJSON

-- | @since 0.2.0.0
newtype Salt = Salt (Argon2.Salt Argon2) deriving newtype (Show)

-- | @since 0.2.0.0
newSalt :: MonadIO m => m Salt
newSalt = Salt <$> Argon2.newSalt

-- | @since 0.2.0.0
newtype Password = Password Argon2.Password deriving newtype (Show, FromJSON)

-- | @since 0.2.0.0
hashPassword :: Password -> Salt -> PasswordHash
hashPassword (Password pw) (Salt s) = PasswordHash $ hashPasswordWithSalt defaultParams s pw

-- | @since 0.2.0.0
checkPassword :: Password -> PasswordHash -> PasswordCheck
checkPassword (Password pw) (PasswordHash ph) = Argon2.checkPassword pw ph

-- | @since 0.2.0.0
deriving via (WithValidation Argon2.Password) instance FromJSON (WithValidation Password)

-- | @since 0.2.0.0
-- Password hashed using Argon2
newtype PasswordHash = PasswordHash (Argon2.PasswordHash Argon2) deriving (Eq, Show)
