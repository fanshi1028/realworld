{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Password where

import Data.Field.Password (Password (..))
import Data.Password.Argon2 (mkPassword)
import Data.Password.Validate (ValidationResult (ValidPassword), defaultPasswordPolicy_, validatePassword)
import Gen.Realistic.Util (Realistic (..))
import Test.QuickCheck (Arbitrary (arbitrary), arbitraryASCIIChar, elements, suchThat)

-- FIXME realistic validated instance?
instance Arbitrary (Realistic Password) where
  arbitrary =
    Realistic . Password
      <$> (mkPassword . fromString <$> (elements [6 .. 15] >>= flip replicateM arbitraryASCIIChar))
        -- <$> (mkPassword <$> arbitrary)
        -- <$> (mkPassword . fromString <$> listOf arbitraryASCIIChar)
        `suchThat` (\pw -> validatePassword defaultPasswordPolicy_ pw == ValidPassword)
