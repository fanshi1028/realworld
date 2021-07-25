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
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Control.Exception.Safe (MonadCatch)
import qualified CurrentTime
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Domain.Util.Error (AlreadyExists, NotAuthorized (NotAuthorized), NotFound, ValidationErr)
import qualified GenID
import HTTP.Authed (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage.InMem.STM
import qualified Tag (E)
import qualified Transform (E)
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
    Member (Transform.E ArticleR "all" "withAuthorProfile") sig,
    Member (Transform.E ArticleR "create" "all") sig,
    Member (Authentication.Token.E UserR) sig,
    Member (Storage.InMem.STM.E UserR) sig,
    Member (Storage.InMem.STM.E ArticleR) sig,
    Member (Storage.InMem.STM.E CommentR) sig,
    Member (GenID.E ArticleR) sig,
    Member (GenID.E CommentR) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member CurrentTime.E sig,
    MonadIO m,
    MonadCatch m
  ) =>
  ServerT Api m
server =
  ( publicServer
      :<|> ( \auth ->
               hoistServer
                 (Proxy @AuthedApi)
                 ( case auth of
                     Authenticated user -> usingReaderT user . UserAction.Pure.run
                     -- FIXME is this undefined ok?
                     _ -> usingReaderT undefined . UserAction.Pure.run . (throwError (NotAuthorized @UserR) >>)
                 )
                 authedServer
           )
  )
    :<|> pure "health-checked"
