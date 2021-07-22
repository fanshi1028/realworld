{-# LANGUAGE DataKinds #-}

-- |
module Authentication where

data E r (m :: Type -> Type) a where
  Login :: r "login" -> E r m (r "id")
  Logout :: E r m ()
