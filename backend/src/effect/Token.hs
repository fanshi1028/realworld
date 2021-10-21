-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for token
--
-- @since 0.1.0.0
module Token (E (..), module X) where

import Authentication (HasAuth (AuthOf))
import Token.Internal.HasToken as X
import Token.Internal.HasToken.User as X

-- | @since 0.2.0.0
data E s (m :: Type -> Type) a where
  -- | Decode token to auth info
  DecodeToken :: TokenOf s -> E s m (AuthOf s)
  -- | Create token from auth info
  CreateToken :: AuthOf s -> E s m (TokenOf s)
  -- | Invalidate token
  InvalidateToken :: TokenOf s -> E s m ()
