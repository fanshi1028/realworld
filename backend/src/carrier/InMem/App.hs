{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Description : App
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- App with effect in memory
--
-- @since 0.2.0.0
module InMem.App where

import Authentication.HasAuth (AlreadyLogin, AuthOf, NotAuthorized, NotLogin)
import qualified Control.Carrier.Error.Church as Church (runError)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import qualified Control.Carrier.State.Strict as S (evalState)
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Returning (runTrace)
import Control.Effect.Labelled (runLabelled)
import Control.Effect.Lift (sendM)
import Control.Exception.Safe (catch)
import Cookie.Xsrf (runCreateXsrfCookie)
import CreateSalt (CreateSaltC (runCreateSalt))
import qualified Crypto.JOSE (Error)
import Crypto.Random (SystemDRG, getSystemDRG)
import Data.UUID.V1 (nextUUID)
import Domain (Domain (Article, Comment, User))
import Field.Email (Email)
import Field.Tag (Tag)
import Field.Time (getCurrentTime)
import HTTP (Api, server)
import InMem.Authentication.User (runAuthenticationUserInMem)
import InMem.OptionalAuthAction (runOptionalAuthActionInMem)
import InMem.OptionalAuthAction.Many (runOptionalAuthActionManyInMem)
import InMem.Relation (ArticleFavoritedByUser, ArticleHasComment, ArticleTaggedByTag, EmailOfUser, TagTagArticle, UserCreateArticle, UserCreateComment, UserFavoriteArticle, UserFollowUser, UserFollowedByUser)
import InMem.Storage (TableInMem)
import InMem.UserAction (runUserActionInMem)
import InMem.UserAction.Many (runUserActionManyInMem)
import InMem.VisitorAction (runVisitorActionInMem)
import Paging (Limit (Limit), Offset (Offset))
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, err404, err500, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Server (err403)
import StmContainers.Map as STM.Map (newIO)
import qualified StmContainers.Map as STM (Map)
import StmContainers.Multimap (Multimap)
import qualified StmContainers.Multimap as STM.Multimap (newIO)
import StmContainers.Set as STM.Set (newIO)
import qualified StmContainers.Set as STM (Set)
import Storage.Error (AlreadyExists, NotFound)
import Storage.Map (CRUD (D, U), Forbidden, IdAlreadyExists, IdNotFound, IdOf)
import Token.Create.JWT (runCreateTokenJWT)
import Token.Decode (InvalidToken)
import Token.HasToken (TokenOf (..))
import Util.Validation (ValidationErr)

-- | @since 0.4.0.0
-- Error runner to throw in memory as 'ServerError' with HTTP status code
runErrors =
  let asStatus :: Show e => (ServerError -> e -> ServerError)
      asStatus status e = status {errBody = show e}
      runThrowInMem f = runThrow >=> either (sendM . throwSTM . f) pure
      runErrorInMem f = Church.runError (sendM . throwSTM . f) pure
   in runThrowInMem (\e -> err500 {errBody = encodeUtf8 @Text e})
        >>> runErrorInMem (asStatus @(Forbidden 'D 'Article) err403)
        >>> runErrorInMem (asStatus @(Forbidden 'U 'Article) err403)
        >>> runErrorInMem (asStatus @(Forbidden 'D 'Comment) err403)
        >>> runErrorInMem (asStatus @(NotAuthorized 'User) err403)
        >>> runErrorInMem (asStatus @(NotLogin 'User) err401)
        >>> runErrorInMem (asStatus @(InvalidToken 'User) err401)
        >>> runErrorInMem (asStatus @(IdNotFound 'Article) err404)
        >>> runErrorInMem (asStatus @(IdNotFound 'Comment) err404)
        >>> runErrorInMem (asStatus @(IdNotFound 'User) err404)
        >>> runErrorInMem (asStatus @(NotFound Email) err404)
        >>> runThrowInMem (asStatus @(NotFound Tag) err404)
        >>> runThrowInMem (asStatus @(IdAlreadyExists 'User) err400)
        >>> runThrowInMem (asStatus @(IdAlreadyExists 'Article) err400)
        >>> runThrowInMem (asStatus @(IdAlreadyExists 'Comment) err500)
        >>> runThrowInMem (asStatus @(AlreadyExists Email) err400)
        >>> runThrowInMem (asStatus @(AlreadyLogin 'User) err400)
        >>> runThrowInMem (asStatus @ValidationErr err400)
        >>> runThrowInMem (asStatus @Crypto.JOSE.Error err500)
{-# INLINEABLE runErrors #-}

-- | @since 0.3.0.0
-- create app by supplying settings and databases(in memory)
mkApp ::
  CookieSettings ->
  JWTSettings ->
  TableInMem 'User ->
  TableInMem 'Article ->
  TableInMem 'Comment ->
  STM.Set Tag ->
  -- | email of user
  STM.Map Email (IdOf 'User) ->
  -- | article has comment
  Multimap (IdOf 'Article) (IdOf 'Comment) ->
  -- | user create article
  Multimap (IdOf 'User) (IdOf 'Article) ->
  -- | article tagged by tag
  Multimap (IdOf 'Article) Tag ->
  -- | tag tag article
  Multimap Tag (IdOf 'Article) ->
  -- | user follow user
  Multimap (IdOf 'User) (IdOf 'User) ->
  -- | user followed by user
  Multimap (IdOf 'User) (IdOf 'User) ->
  -- | user favourite article
  Multimap (IdOf 'User) (IdOf 'Article) ->
  -- | article favourited by user
  Multimap (IdOf 'Article) (IdOf 'User) ->
  -- | user created comment
  Multimap (IdOf 'User) (IdOf 'Comment) ->
  Application
mkApp cs jwts userDb articleDb commentDb tagDb emailUserIndex db0 db1 db2 db3 db4 db5 db6 db7 db8 =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( \eff -> do
          time <- liftIO getCurrentTime
          uuid <- liftIO nextUUID >>= maybe (throwError $ err500 {errBody = "RequestedUUIDsTooQuickly"}) pure
          gen <- liftIO getSystemDRG
          let runStorageInMem =
                runUserActionManyInMem @[]
                  >>> runUserActionInMem
                  >>> runOptionalAuthActionManyInMem @[]
                  >>> runOptionalAuthActionInMem
                  >>> runVisitorActionInMem @[]
                  >>> runAuthenticationUserInMem
                  >>> (runLabelled @UserCreateComment >>> R.runReader db8)
                  >>> (runLabelled @ArticleFavoritedByUser >>> R.runReader db7)
                  >>> (runLabelled @UserFavoriteArticle >>> R.runReader db6)
                  >>> (runLabelled @UserFollowedByUser >>> R.runReader db5)
                  >>> (runLabelled @UserFollowUser >>> R.runReader db4)
                  >>> (runLabelled @TagTagArticle >>> R.runReader db3)
                  >>> (runLabelled @ArticleTaggedByTag >>> R.runReader db2)
                  >>> (runLabelled @UserCreateArticle >>> R.runReader db1)
                  >>> (runLabelled @ArticleHasComment >>> R.runReader db0)
                  >>> (runLabelled @EmailOfUser >>> R.runReader emailUserIndex)
                  >>> R.runReader userDb
                  >>> R.runReader articleDb
                  >>> R.runReader commentDb
                  >>> R.runReader tagDb
          ( eff
              & runStorageInMem
              & runCreateTokenJWT @'User @SystemDRG
              & runCreateXsrfCookie @SystemDRG
              & runCreateSalt @SystemDRG
              & S.evalState gen
              & R.runReader uuid
              & R.runReader time
              & R.runReader jwts
              & R.runReader cs
              & R.runReader (Limit 20)
              & R.runReader (Offset 0)
              & R.runReader (Nothing @(AuthOf 'User))
              & R.runReader (Nothing @(TokenOf 'User))
              & runErrors
              & runTrace
              & runM
              <&> snd
            )
            & atomically
            & (`catch` throwError)
      )
      server

-- | @since 0.2.0.0
-- create in-memory app with default settings
newApp :: IO Application
newApp =
  mkApp defaultCookieSettings . defaultJWTSettings
    <$> generateKey
    <*> STM.Map.newIO
    <*> STM.Map.newIO
    <*> STM.Map.newIO
    <*> STM.Set.newIO
    <*> STM.Map.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
    <*> STM.Multimap.newIO
