{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of authentication
--
-- @since 0.1.0.0
module Authentication
  ( E (..),

    -- * Error

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

import Authentication.Internal.HasAuth as X
import Authentication.Internal.HasAuth.User as X
import Domain (Domain)
import InMem.Storage.Map.Internal.HasCreate (CreateOf)

-- | @since 0.3.0.0
data E (s :: Domain) (m :: Type -> Type) a where
  -- | @since 0.2.0.0
  Register :: CreateOf s -> E s m (AuthOf s)
  -- | @since 0.2.0.0
  Login :: LoginOf s -> E s m (AuthOf s)

-- | @since 0.2.0.0
newtype AlreadyLogin (a :: Domain) = AlreadyLogin (LoginOf a)

-- | @since 0.2.0.0
deriving instance Show (LoginOf a) => Show (AlreadyLogin a)

-- | @since 0.3.0.0
data NotAuthorized (a :: Domain) = BadPassword | NoSuchUser

-- | @since 0.2.0.0
deriving instance Show (LoginOf a) => Show (NotAuthorized a)

-- | @since 0.2.0.0
data NotLogin (a :: Domain) = NotLogin deriving (Show)
