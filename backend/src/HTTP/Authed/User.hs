{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.User (UserApi, userServer) where

import Domain.User (UserR)
import HTTP.Util (ReadApi, UpdateApi)
import Servant (ServerT, type (:<|>) ((:<|>)))

type UserApi = ReadApi UserR "authWithToken" :<|> UpdateApi UserR "authWithToken"

-- FIXME
userServer :: ServerT UserApi m
userServer = undefined :<|> undefined
