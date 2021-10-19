{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to generate UUID
--
-- @since 0.1.0.0
module GenUUID (E (..)) where

import Data.UUID (UUID)

-- | since 0.1.0.0
data E (m :: Type -> Type) a where
  -- | generate UUID
  Generate :: E m UUID
