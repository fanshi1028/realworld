{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP (server, Api) where

import qualified Authentication (E)
import qualified Authentication.Token (E)
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E)
import qualified Current.Reader (run)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Domain.Util.Error (AlreadyExists, Impossible, NotAuthorized (NotAuthorized), NotFound, ValidationErr)
import Domain.Util.Field (Time)
import qualified GenUUID (E)
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import qualified Relation.ManyToMany (E)
import qualified Relation.OneToMany (E)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage.Map (E)
import qualified UserAction (run)
import qualified VisitorAction (E)

type Api =
  "api"
    :> (PublicApi :<|> Auth '[TokenAuth] (UserR "authWithToken") :> AuthedApi)
    :<|> Get '[JSON] Text

server ::
  ( Algebra sig m,
    Member (Lift IO) sig,
    Member GenUUID.E sig,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member (Throw Impossible) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Authentication.Token.E UserR) sig,
    Member (Authentication.E UserR) sig,
    Member (Current.E Time) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Storage.Map.E ArticleR) sig,
    Member (Storage.Map.E CommentR) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.OneToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Relation.OneToMany.E (UserR "id") "create" (ArticleR "id")) sig
  ) =>
  ServerT Api m
server =
  ( publicServer
      :<|> ( \auth ->
               hoistServer
                 (Proxy @AuthedApi)
                 ( case auth of
                     Authenticated user -> usingReaderT user . Current.Reader.run . UserAction.run
                     -- HACK FIXME is this undefined ok?
                     _ -> usingReaderT undefined . Current.Reader.run . UserAction.run . (throwError (NotAuthorized @UserR) >>)
                 )
                 authedServer
           )
  )
    :<|> pure "health-checked"
