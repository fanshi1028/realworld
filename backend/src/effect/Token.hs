{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for token
--
-- @since 0.1.0.0
module Token
  ( E (..),

    -- * Error

    -- ** Invalid token
    InvalidToken (..),

    -- * Typeclass

    -- ** HasToken
    module X,
  )
where

import Authentication (HasAuth (AuthOf))
import Domain (Domain)
import Token.Internal.HasToken as X
import Token.Internal.HasToken.User as X

-- | @since 0.2.0.0
data E s (m :: Type -> Type) a where
  -- | @since 0.2.0.0
  -- Decode token to auth info
  DecodeToken :: TokenOf s -> E s m (AuthOf s)
  -- | @since 0.2.0.0
  -- Create token from auth info
  CreateToken :: AuthOf s -> E s m (TokenOf s)
  -- | @since 0.2.0.0
  -- Invalidate token
  InvalidateToken :: TokenOf s -> E s m ()

-- | @since 0.2.0.0
newtype InvalidToken (a :: Domain) = InvalidToken (TokenOf a)

-- | @since 0.2.0.0
deriving instance Show (TokenOf a) => Show (InvalidToken a)
