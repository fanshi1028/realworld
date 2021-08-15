{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP (server, Api) where

import qualified Authentication.Token (E)
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain.User (UserR)
import Domain.Util.Error (Impossible, NotAuthorized, ValidationErr)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult, CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified UserAction (E)
import qualified VisitorAction (E)

type Api =
  "api"
    :> (Auth '[TokenAuth] (UserR "authWithToken") :> (PublicApi :<|> AuthedApi))
    :<|> Get '[JSON] Text

server ::
  ( Algebra sig m,
    Member (Catch (NotAuthorized UserR)) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Lift IO) sig,
    Member VisitorAction.E sig,
    Member UserAction.E sig,
    Member (Throw ValidationErr) sig,
    Member (R.Reader (AuthResult (UserR "authWithToken"))) sig,
    Member (Authentication.Token.E UserR) sig,
    Member (Throw Impossible) sig
  ) =>
  ServerT Api m
server =
  ( \auth ->
      hoistServer
        (Proxy @(PublicApi :<|> AuthedApi))
        (R.local $ const auth)
        (publicServer :<|> authedServer)
  )
    :<|> pure "health-checked"
