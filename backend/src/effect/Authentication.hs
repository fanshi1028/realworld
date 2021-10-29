{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of authentication
--
-- @since 0.1.0.0
module Authentication (E (..), module X) where

import Authentication.Internal.HasAuth as X
import Authentication.Internal.HasAuth.User as X
import Domain (Domain)
import Storage.Map (CreateOf)

-- | @since 0.2.0.0
data E (s :: Domain) (m :: Type -> Type) a where
  -- | @since 0.2.0.0
  Register :: CreateOf s -> E s m (AuthOf s)
  -- | @since 0.2.0.0
  Login :: LoginOf s -> E s m (AuthOf s)
  -- | @since 0.2.0.0
  Logout :: E s m ()
