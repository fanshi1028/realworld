{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.User (UserApi, userServer) where

import Domain.User (UserR)
import HTTP.Util (ReadApi, UpdateApi)
import Servant (Server, type (:<|>))

type UserApi = ReadApi UserR "auth" :<|> UpdateApi UserR "auth"

-- FIXME
userServer :: Server UserApi
userServer = undefined
