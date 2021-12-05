{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for creating in storage
--
-- @since 0.2.0.0
module InMem.Storage.Map.Internal.HasCreate where

import Domain (Domain)

-- | @since 0.2.0.0
class HasCreate (s :: Domain) where
  -- | @since 0.2.0.0
  -- Type for creation
  data CreateOf s
