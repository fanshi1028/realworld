{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass for effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- typeclass for implementing for storage effect
--
-- @since 0.2.0.0
module Storage.Map.Internal.HasStorage where

import GHC.TypeLits (Symbol)

-- @since 0.2.0.0
class HasStorage (s :: Symbol) where
  -- | need an id for indexing in storage
  data IdOf s

  -- | stuff to store
  data ContentOf s

  -- | input to store
  data InputOf s
