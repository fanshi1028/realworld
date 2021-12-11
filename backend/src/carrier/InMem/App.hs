{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Authentication.HasAuth (AlreadyLogin, NotAuthorized, NotLogin)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import qualified Control.Carrier.State.Strict as S (evalState)
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Returning (runTrace)
import Control.Effect.Labelled (runLabelled)
import Control.Exception.Safe (catch)
import qualified Cookie.Xsrf (run)
import qualified Crypto.JOSE (Error)
import Crypto.JWT (SystemDRG, getSystemDRG)
import Data.UUID.V1 (nextUUID)
import Domain (Domain (Article, Comment, User))
import Domain.User (UserR)
import Field.Email (Email)
import Field.Tag (Tag)
import Field.Time (getCurrentTime)
import HTTP (Api, server)
import qualified InMem.Authentication.User (run)
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
          ( eff
              & runUserActionManyInMem @[]
              & runUserActionInMem @SystemDRG
              & runOptionalAuthActionManyInMem @[]
              & runOptionalAuthActionInMem
              & runVisitorActionInMem @[]
              & runCreateTokenJWT @'User @SystemDRG
              & Cookie.Xsrf.run @SystemDRG
              & InMem.Authentication.User.run @SystemDRG
              & (runLabelled @UserCreateComment >>> R.runReader db8)
              & (runLabelled @ArticleFavoritedByUser >>> R.runReader db7)
              & (runLabelled @UserFavoriteArticle >>> R.runReader db6)
              & (runLabelled @UserFollowedByUser >>> R.runReader db5)
              & (runLabelled @UserFollowUser >>> R.runReader db4)
              & (runLabelled @TagTagArticle >>> R.runReader db3)
              & (runLabelled @ArticleTaggedByTag >>> R.runReader db2)
              & (runLabelled @UserCreateArticle >>> R.runReader db1)
              & (runLabelled @ArticleHasComment >>> R.runReader db0)
              & (runLabelled @EmailOfUser >>> R.runReader emailUserIndex)
              & R.runReader userDb
              & R.runReader articleDb
              & R.runReader commentDb
              & R.runReader tagDb
              & R.runReader uuid
              & R.runReader time
              & R.runReader jwts
              & R.runReader cs
              & R.runReader (Limit 20)
              & R.runReader (Offset 0)
              & R.runReader (Nothing @(UserR "authWithToken"))
              & S.evalState gen
              & runTrace
              & runThrow @Text
              & runError @(Forbidden 'D 'Article)
              & runError @(Forbidden 'U 'Article)
              & runError @(Forbidden 'D 'Comment)
              & runError @(NotAuthorized 'User)
              & runError @(NotLogin 'User)
              & runError @(InvalidToken 'User)
              & runError @(IdNotFound 'Article)
              & runError @(IdNotFound 'Comment)
              & runError @(IdNotFound 'User)
              & runError @(NotFound Email)
              & runThrow @(NotFound Tag)
              & runThrow @(IdAlreadyExists 'User)
              & runThrow @(IdAlreadyExists 'Article)
              & runThrow @(IdAlreadyExists 'Comment)
              & runThrow @(AlreadyExists Email)
              & runThrow @(AlreadyLogin 'User)
              & runThrow @ValidationErr
              & runThrow @Crypto.JOSE.Error
              & runM
              >>= handlerErr err500
              >>= handlerErr err400
              >>= handlerErr err400
              >>= handlerErr err400 -- ???? FIXME
              >>= handlerErr err500 -- Comment AlreadyExists use 500
              >>= handlerErr err400
              >>= handlerErr err400
              >>= handlerErr err404
              >>= handlerErr err404
              >>= handlerErr err404
              >>= handlerErr err404
              >>= handlerErr err404
              >>= handlerErr err401
              >>= handlerErr err401
              >>= handlerErr err403
              >>= handlerErr err403
              >>= handlerErr err403
              >>= handlerErr err403
              >>= handlerErr err500
              <&> snd
            )
            & atomically
            & (`catch` throwError)
      )
      server
  where
    -- NOTE: Helpers for handle errors in form of nested either
    handlerErr' handler = either handler pure
    handlerErr status = handlerErr' (\e -> throwSTM $ status {errBody = show e})

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
