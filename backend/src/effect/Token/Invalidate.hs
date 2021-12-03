{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for invalidating token
--
-- @since 0.3.0.0
module Token.Invalidate where

import Token (TokenOf)

-- | @since 0.3.0.0
data E s (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Invalidate token
  InvalidateToken :: TokenOf s -> E s m ()