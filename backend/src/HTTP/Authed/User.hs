{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.User (UserApi, userServer) where

import Domain.User (UserR)
import HTTP.Util (ReadApi, UpdateApi)
import Servant (type (:<|>), ServerT)

type UserApi = ReadApi UserR "auth" :<|> UpdateApi UserR "auth"

-- FIXME
userServer :: ServerT UserApi m
userServer = undefined
