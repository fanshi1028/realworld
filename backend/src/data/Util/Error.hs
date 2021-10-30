{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Description : Type
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some Types for some common errors.
--
-- @since 0.1.0.0
module Util.Error where

-- | @since 0.1.0.0
newtype NotAuthorized a = NotAuthorized a deriving (Show)

-- | @since 0.1.0.0
newtype AlreadyLogin a = AlreadyLogin a deriving (Show)

-- | @since 0.1.0.0
newtype NotLogin a = NotLogin a deriving (Show)

-- | @since 0.1.0.0
-- Use it to mark the impossible error case
newtype Impossible
  = -- | 'Text' to provide context, in case the __IMPOSSIBLE__ really happens
    Impossible Text
  deriving (Show)

-- | @since 0.1.0.0
-- Just use 'Text' to represent all validation errors. When it happens, there will be a nonempty list of them.
type ValidationErr = NonEmpty Text
