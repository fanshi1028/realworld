{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of authentication
--
-- @since 0.3.0.0
module Authentication
  ( AuthenticationE (..),
  )
where

import Authentication.HasAuth (HasAuth (AuthOf, LoginOf))
import Domain (Domain)
import Storage.Map (CreateOf)

-- | @since 0.3.0.0
data AuthenticationE (s :: Domain) (m :: Type -> Type) a where
  -- | @since 0.2.0.0
  Register :: CreateOf s -> AuthenticationE s m (AuthOf s)
  -- | @since 0.2.0.0
  Login :: LoginOf s -> AuthenticationE s m (AuthOf s)
  -- | @since 0.4.0.0
  -- Get the info of the current authenticated user.
  GetCurrentAuth :: AuthenticationE s m (Maybe (AuthOf s))
