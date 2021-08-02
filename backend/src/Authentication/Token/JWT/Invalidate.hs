{-# LANGUAGE DataKinds #-}

-- |
module Authentication.Token.JWT.Invalidate where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  Invalidate :: r "token" -> E r m ()