{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public (PublicApi, publicServer) where

import Control.Algebra (Algebra)
import Control.Effect.Lift (Lift)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain.Util.Error (ValidationErr)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, profileServer)
import HTTP.Public.Tag (TagApi, tagServer)
import HTTP.Public.User (UserApi, userServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified VisitorAction (E)

type PublicApi =
  "users" :> UserApi
    :<|> "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "tags" :> TagApi

publicServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Lift IO) sig
  ) =>
  ServerT PublicApi m
publicServer = userServer :<|> profileServer :<|> articleServer :<|> tagServer
