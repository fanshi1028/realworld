{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP (server, Api) where

import qualified Authentication
import qualified Authentication.Token
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified CurrentTime
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Domain.Util.Error (AlreadyExists, NotAuthorized (NotAuthorized), NotFound, ValidationErr)
import qualified GenID
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import qualified Relation
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage
import qualified Tag (E)
import qualified UserAction.Pure
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
    Member (Authentication.Token.E UserR) sig,
    Member (GenID.E ArticleR) sig,
    Member (GenID.E CommentR) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member CurrentTime.E sig,
    Member (Relation.E ArticleR "id" CommentR "id" HashSet) sig,
    Member (Storage.E UserR) sig,
    Member (Storage.E ArticleR) sig,
    Member (Storage.E CommentR) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member (Lift IO) sig
  ) =>
  ServerT Api m
server =
  ( publicServer
      :<|> ( \auth ->
               hoistServer
                 (Proxy @AuthedApi)
                 ( case auth of
                     Authenticated user -> usingReaderT user . UserAction.Pure.run
                     -- HACK FIXME is this undefined ok?
                     _ -> usingReaderT undefined . UserAction.Pure.run . (throwError (NotAuthorized @UserR) >>)
                 )
                 authedServer
           )
  )
    :<|> pure "health-checked"
