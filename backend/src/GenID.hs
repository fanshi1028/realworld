{-# LANGUAGE DataKinds #-}

-- |
module GenID where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  GenerateID :: r "create" -> E r m (r "id")
