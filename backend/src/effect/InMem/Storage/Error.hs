{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Error
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Common error for storage effect
--
-- @since 0.3.0.0
module InMem.Storage.Error where

import Data.Aeson (ToJSON)

-- * Already exists

-- | @since 0.3.0.0
-- error when a has already existed
newtype AlreadyExists a = AlreadyExists a deriving (Show)

-- * Not found

-- | @since 0.3.0.0
-- when a is not found
newtype NotFound a = NotFound a deriving (Show)

-- | @since 0.3.0.0
deriving instance (ToJSON a, Show a) => ToJSON (NotFound a)
