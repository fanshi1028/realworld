{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for invalidate JWT token
--
-- @since 0.1.0.0
module Token.JWT.Invalidate (E (..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  -- | Invalidate JWT token
  Invalidate :: r "token" -> E r m ()
