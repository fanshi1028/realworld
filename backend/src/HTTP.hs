{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP (server, Api) where

import Authorization.TokenAuth (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as C
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized (NotAuthorized), ValidationErr)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated))
import Servant.Server (hoistServer)
import qualified Tag (E)
import qualified VisitorAction (E)
import qualified VisitorAction.Batch (E)

type Api =
  "api"
    :> ( PublicApi
           :<|> Auth '[TokenAuth] (UserR "id") :> AuthedApi
       )
    :<|> Get '[JSON] Text

server ::
  ( Algebra sig m,
    Member (Tag.E []) sig,
    Member VisitorAction.E sig,
    Member (VisitorAction.Batch.E []) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotAuthorized UserR)) sig
  ) =>
  ServerT Api m
server =
  ( publicServer
      :<|> ( \auth ->
               hoistServer
                 (Proxy @AuthedApi)
                 ( case auth of
                     Authenticated userId -> C.runReader userId
                     _ -> C.runReader undefined . (throwError (NotAuthorized @UserR) >>)
                 )
                 authedServer
           )
  )
    :<|> pure "health-checked"
