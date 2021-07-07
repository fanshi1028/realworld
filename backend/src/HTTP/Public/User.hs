{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Public.User (UserApi, userServer) where

import Domain.User (UserR)
import Domain.Util.JSON.From (In)
import Domain.Util.JSON.To (Out)
import HTTP.Util (CreateApi)
import Servant (JSON, Post, ReqBody, type (:<|>), type (:>))
import Servant.Server (Server)
import Validation.Carrier.Selective (WithValidation)

type UserApi =
  "login" :> ReqBody '[JSON] (In (WithValidation (UserR "login"))) :> Post '[JSON] (Out (UserR "auth"))
    :<|> CreateApi UserR "auth"

-- FIXME
userServer :: Server UserApi
userServer = undefined
