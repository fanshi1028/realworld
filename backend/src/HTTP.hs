{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP (server, Api) where

import Authorization.TokenAuth (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, runReader)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized (NotAuthorized), ValidationErr)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Tag (E)
import qualified Transform
import qualified VisitorAction (E)
import qualified VisitorAction.Batch (E)

type Api =
  "api"
    :> (PublicApi :<|> Auth '[TokenAuth] (UserR "authWithToken") :> AuthedApi)
    :<|> Get '[JSON] Text

server ::
  ( Algebra sig m,
    Member (Tag.E []) sig,
    Member VisitorAction.E sig,
    Member (VisitorAction.Batch.E []) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Transform.E UserR "auth" "authWithToken") sig,
    MonadIO m
  ) =>
  ServerT Api m
server =
  ( publicServer
      :<|> ( \auth ->
               hoistServer
                 (Proxy @AuthedApi)
                 ( case auth of
                     Authenticated userId -> R.runReader userId
                     _ -> R.runReader undefined . (throwError (NotAuthorized @UserR) >>)
                 )
                 authedServer
           )
  )
    :<|> pure "health-checked"
