{-# LANGUAGE DataKinds #-}

-- |
module Authentication (E (..)) where

data E r (m :: Type -> Type) a where
  Register :: r "create" -> E r m (r "auth")
  Login :: r "login" -> E r m (r "auth")
  Logout :: E r m ()
