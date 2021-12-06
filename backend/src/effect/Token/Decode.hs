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
-- Effect for decoding token
--
-- @since 0.3.0.0
module Token.Decode
  ( E (..),

    -- * Error

    -- ** Invalid token
    InvalidToken (..),
  )
where

import Authentication.HasAuth (AuthOf)
import Domain (Domain)
import Token.HasToken (TokenOf)

-- | @since 0.3.0.0
data E s (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Decode token to auth info
  DecodeToken :: TokenOf s -> E s m (AuthOf s)

-- | @since 0.3.0.0
newtype InvalidToken (a :: Domain) = InvalidToken (TokenOf a)

-- | @since 0.3.0.0
deriving instance Show (TokenOf a) => Show (InvalidToken a)
