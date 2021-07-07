{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP (server, Api) where

import Domain.User (UserR)
import Domain.Util.Field (Tag)
import Domain.Util.JSON.To (Out)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import HTTP.Public.Profile (ProfileServerEffect)
import HTTP.Public.Tag (TagServerEffect)
import HTTP.Util (EffRunner)
import Servant (Get, JSON, Server, type (:<|>) ((:<|>)), type (:>))

type Api =
  "api"
    :> ( PublicApi
           -- FIXME: need auth
           :<|> AuthedApi
       )
    :<|> Get '[JSON] Text

server ::
  ( ProfileServerEffect sig1 m,
    TagServerEffect sig2 n
  ) =>
  EffRunner m (Out (UserR "profile")) ->
  EffRunner n (Out [Tag]) ->
  Server Api
server profileC tagC = (publicServer profileC tagC :<|> authedServer) :<|> pure "health-checked"
