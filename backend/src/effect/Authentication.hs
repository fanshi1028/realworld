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
import Storage.Map (CreateOf)
import Domain (Domain)

-- | @since 0.2.0.0
data E (s :: Domain) (m :: Type -> Type) a where
  Register :: CreateOf s -> E s m (AuthOf s)
  Login :: LoginOf s -> E s m (AuthOf s)
  Logout :: E s m ()
