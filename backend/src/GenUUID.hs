{-# LANGUAGE DataKinds #-}

-- |
module GenUUID where

import Data.UUID (UUID)

data E (m :: Type -> Type) a where
  Generate :: E m UUID
