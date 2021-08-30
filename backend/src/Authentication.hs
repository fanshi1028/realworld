{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of authentication
--
-- @since 0.1.0.0
module Authentication where

-- | @since 0.1.0.0
data E r (m :: Type -> Type) a where
  Register :: r "create" -> E r m (r "auth")
  Login :: r "login" -> E r m (r "auth")
  Logout :: E r m ()
