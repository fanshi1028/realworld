{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP (server, Api) where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import VisitorAction.Effect (VisitorAction)
import qualified Tag (E)

type Api =
  "api"
    :> ( PublicApi
           -- FIXME: need auth
           :<|> AuthedApi
       )
    :<|> Get '[JSON] Text

server ::
  ( Algebra sig m,
    Member VisitorAction sig
    Member (Tag.E []) sig
  ) =>
  ServerT Api m
server = (publicServer :<|> authedServer) :<|> pure "health-checked"
