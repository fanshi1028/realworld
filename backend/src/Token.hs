{-# LANGUAGE DataKinds #-}

-- |
module Token (E (..)) where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  CheckToken :: r "token" -> E r m (r "auth")
  CreateToken :: r "auth" -> E r m (r "token")
  InvalidateToken :: r "token" -> E r m ()
