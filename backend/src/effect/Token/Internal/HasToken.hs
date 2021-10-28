{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for auth token
--
-- @since 0.2.0.0
module Token.Internal.HasToken where

import Authentication (HasAuth)

-- | @since 0.2.0.0
class HasAuth s => HasToken s where
  -- | @since 0.2.0.0
  -- Type of the token
  data TokenOf s
