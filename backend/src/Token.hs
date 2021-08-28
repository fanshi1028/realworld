{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for token
--
-- @since 0.1.0.0
module Token (E (..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  -- | Decode token to auth info
  DecodeToken :: r "token" -> E r m (r "auth")
  -- | Create token from auth info
  CreateToken :: r "auth" -> E r m (r "token")
  -- | Invalidate token
  InvalidateToken :: r "token" -> E r m ()
