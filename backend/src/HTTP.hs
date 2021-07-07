{-# LANGUAGE DataKinds #-}

-- |
module HTTP (app) where

import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Application, Get, JSON, Server, serve, type (:<|>) ((:<|>)), type (:>))
import Tag.Carrier.Pure (TagPure (runTagPure))
import Control.Algebra (run)

type Api =
  "api"
    :> ( PublicApi
           -- FIXME: need auth
           :<|> AuthedApi
       )
    :<|> Get '[JSON] Text

server :: Server Api
server = (publicServer :<|> authedServer) :<|> pure "health-checked"

app :: Application
app = serve (Proxy @Api) server
