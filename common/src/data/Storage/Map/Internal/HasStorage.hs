{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for storage
--
-- @since 0.4.0.0
module Data.Storage.Map.Internal.HasStorage where

import Data.Domain (Domain)

-- | @since 0.3.0.0
class HasStorage (s :: Domain) where
  -- | @since 0.3.0.0
  -- Type for id in storage
  data IdOf s

  -- | @since 0.3.0.0
  -- Type for content in storage
  data ContentOf s
