{-# OPTIONS_GHC -Wno-orphans #-}

-- | @sicne 0.4.1.0
module Orphans.Internal.Data.Field.Password.Eq where

import Data.Field.Password (Password (Password))
import Data.Password.Argon2 (unsafeShowPassword)

-- | Password
--
-- @since 0.2.0.0
instance Eq Password where
  (==) = (==) `on` (\(Password pw) -> unsafeShowPassword pw)
