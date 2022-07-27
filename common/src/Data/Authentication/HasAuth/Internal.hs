{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for auth
--
-- @since 0.4.0.0
module Data.Authentication.HasAuth.Internal where

import Data.Domain (Domain)

-- | @since 0.2.0.0
class HasAuth (s :: Domain) where
  -- | @since 0.2.0.0
  -- Type for login
  data LoginOf s

  -- | @since 0.2.0.0
  -- Type of auth data
  data AuthOf s
