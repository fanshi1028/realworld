{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for invalidating token
--
-- @since 0.4.0.0
module Effect.Token.Invalidate where

import Data.Token.HasToken (TokenOf)

-- | @since 0.3.0.0
data InvalidateTokenE s (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Invalidate token
  InvalidateToken :: TokenOf s -> InvalidateTokenE s m ()
