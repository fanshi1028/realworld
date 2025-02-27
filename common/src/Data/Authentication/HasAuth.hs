{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- HasAuth Typeclass and Error
--
-- @since 0.4.0.0
module Data.Authentication.HasAuth
  ( -- * Error

    -- ** Not Authorized
    NotAuthorized (..),

    -- ** Not login
    NotLogin (..),

    -- ** Already login
    AlreadyLogin (..),

    -- * Typeclass

    -- ** HasAuth
    module X,
  )
where

import Data.Authentication.HasAuth.Internal as X
import Data.Authentication.HasAuth.Internal.User as X
import Data.Domain (Domain)

-- | @since 0.3.0.0
newtype AlreadyLogin (a :: Domain) = AlreadyLogin (LoginOf a)

-- | @since 0.3.0.0
deriving instance Show (LoginOf a) => Show (AlreadyLogin a)

-- | @since 0.3.0.0
data NotAuthorized (a :: Domain) = BadPassword | NoSuchUser

-- | @since 0.3.0.0
deriving instance Show (LoginOf a) => Show (NotAuthorized a)

-- | @since 0.3.0.0
data NotLogin (a :: Domain) = NotLogin deriving (Show)
