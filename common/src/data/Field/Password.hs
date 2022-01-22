{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Password
--
-- @since 0.4.0.0
module Data.Field.Password (Password (Password), PasswordHash (PasswordHash), Salt (Salt), hashPassword, checkPassword, newSalt) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.Types (withText)
import Data.Password.Argon2 (Argon2, PasswordCheck, defaultParams, hashPasswordWithSalt, mkPassword)
import qualified Data.Password.Argon2 as Argon2 (Password, PasswordHash, Salt (Salt), checkPassword)
import Data.Password.Validate (ValidationResult (InvalidPassword, ValidPassword), defaultPasswordPolicy_, validatePassword)
import Data.Util.Validation (WithValidation)
import qualified Validation as V (Validation (Failure, Success))

-- | @since 0.2.0.0
newtype Salt = Salt (Argon2.Salt Argon2) deriving newtype (Show)

-- | @since 0.2.0.0
newSalt :: ByteString -> Salt
newSalt = Salt . Argon2.Salt

-- | @since 0.2.0.0
newtype Password = Password Argon2.Password deriving newtype (Show)

-- | @since 0.3.0.0
instance FromJSON Password where
  parseJSON = Password . mkPassword <<$>> parseJSON

-- | @since 0.3.0.0
-- Validate password with default password policy
instance FromJSON (WithValidation Password) where
  parseJSON =
    withText "password" $ \(mkPassword -> pw) -> pure $
      case validatePassword defaultPasswordPolicy_ pw of
        ValidPassword -> V.Success $ Password pw
        InvalidPassword irs ->
          maybe (error "impossible: password must be invalidated with a reason!") V.Failure $
            nonEmpty $ show @Text <$> irs
-- ^
-- >>> import Data.Aeson (eitherDecode')
--
-- ==== Success
-- >>> eitherDecode' @(WithValidation Password) "\"jfoj9343f43f43f\""
-- Right (Success **PASSWORD**)
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Password) "\"jf\""
-- Right (Failure ("PasswordTooShort 8 2" :| []))
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation Password) "{}"
-- Left "Error in $: parsing password failed, expected String, but encountered Object"

-- | @since 0.2.0.0
hashPassword :: Password -> Salt -> PasswordHash
hashPassword (Password pw) (Salt s) = PasswordHash $ hashPasswordWithSalt defaultParams s pw

-- | @since 0.2.0.0
checkPassword :: Password -> PasswordHash -> PasswordCheck
checkPassword (Password pw) (PasswordHash ph) = Argon2.checkPassword pw ph

-- | @since 0.2.0.0
-- Password hashed using Argon2
newtype PasswordHash = PasswordHash (Argon2.PasswordHash Argon2) deriving (Eq, Show)
