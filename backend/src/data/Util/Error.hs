{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some Types for some common errors.
--
-- @since 0.1.0.0
module Util.Error where

import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding.Internal (Encoding' (Encoding))

-- | @since 0.1.0.0
newtype NotFound a
  = -- | when a is not found
    NotFound a
  deriving (Show, Generic)

instance (ToJSON a, Show a) => ToJSON (NotFound a) where
  toEncoding = Encoding . show

-- | @since 0.1.0.0
newtype AlreadyExists a
  = -- | when a has already existed
    AlreadyExists a
  deriving (Show)

-- | @since 0.2.0.0
data CRUD = C | R | U | D deriving (Show)

-- | @since 0.2.0.0
data Forbidden a
  = Forbidden CRUD a
  deriving (Show)

-- | @since 0.1.0.0
newtype NotAuthorized a = NotAuthorized a deriving (Show)

-- | @since 0.1.0.0
newtype AlreadyLogin a = AlreadyLogin a deriving (Show)

-- | @since 0.1.0.0
newtype NotLogin a = NotLogin a deriving (Show)

-- | Use it to mark the impossible error case
--
-- @since 0.1.0.0
newtype Impossible
  = -- | 'Text' to provide context, in case the __IMPOSSIBLE__ really happens
    Impossible Text
  deriving (Show)

-- | Just use 'Text' to represent all validation errors. When it happens, there will be a nonempty list of them.
--
-- @since 0.1.0.0
type ValidationErr = NonEmpty Text