{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP (server, Api) where

import Domain.Util.Field (Tag)
import Domain.Util.JSON.To (Out)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
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
  (TagServerEffect sig m
  ) =>
  EffRunner m (Out [Tag]) ->
  Server Api
server tagC = (publicServer tagC :<|> authedServer) :<|> pure "health-checked"
