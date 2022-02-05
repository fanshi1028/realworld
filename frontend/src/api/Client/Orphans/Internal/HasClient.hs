{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
module Client.Orphans.Internal.HasClient where

import API.Authorization (TokenAuth)
import Data.Domain (Domain (User))
import Data.Sequence ((<|))
import Data.Token.HasToken (TokenOf (UserToken))
import Network.HTTP.Types (hAuthorization)
import Servant.API (type (:>))
import Servant.Auth.Server (Auth)
import Servant.Client (HasClient (Client, clientWithRoute, hoistClientMonad))
import Servant.Client.Core (requestHeaders)

class TokenAuthNotEnabled

-- | For supporting TokenAuth client generation's instance
type family HasTokenAuth xs :: Constraint where
  HasTokenAuth '[TokenAuth] = ()
  HasTokenAuth (x ': xs) = HasTokenAuth xs
  HasTokenAuth '[] = TokenAuthNotEnabled

instance (HasTokenAuth auths, HasClient m api) => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = TokenOf 'User -> Client m api
  clientWithRoute m _ req (UserToken t) =
    clientWithRoute m (Proxy :: Proxy api) $
      req
        { requestHeaders =
            (hAuthorization, "Token " <> t) <| requestHeaders req
        }
  hoistClientMonad p _ f cl = hoistClientMonad p (Proxy :: Proxy api) f . cl
