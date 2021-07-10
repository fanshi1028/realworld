{-# LANGUAGE DataKinds #-}

-- |
module Authorization where

data E r (m :: Type -> Type) a where
  Register :: r "register" -> E r m (r "auth")
  Login :: r "login" -> E r m (Maybe (r "auth"))
  Logout :: E r m Bool
  GetAuthInfo :: E r m (Maybe (r "auth"))
