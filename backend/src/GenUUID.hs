{-# LANGUAGE DataKinds #-}

-- |
module GenUUID (E(..)) where

import Data.UUID (UUID)

data E (m :: Type -> Type) a where
  Generate :: E m UUID
