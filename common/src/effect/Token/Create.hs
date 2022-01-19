-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for creating token
--
-- @since 0.3.0.0
module Token.Create where

import Authentication.HasAuth (AuthOf)
import Token.HasToken (TokenOf)

-- | @since 0.3.0.0
data CreateTokenE s (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Create token from auth info
  CreateToken :: AuthOf s -> CreateTokenE s m (TokenOf s)
