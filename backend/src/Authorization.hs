{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Authorization where

import GHC.TypeLits (Symbol)
import Network.Wai (Request)
import Servant.Auth.Server (AuthResult)

data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  AuthCheck :: Request -> E r m (AuthResult (r "authWithToken"))
