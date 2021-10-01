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
module App.InMem where

import qualified Authentication.Token (run)
import Control.Carrier.Error.Either (runError)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Returning (runTrace)
import qualified Crypto.JOSE (Error)
import qualified Current.IO (run)
import qualified Current.Reader (run)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists (AlreadyExists), AlreadyLogin, Impossible, NotAuthorized, NotFound, NotLogin, ValidationErr, Forbidden (Forbidden))
import Domain.Util.Field (Email, Tag, Time)
import GenUUID.V1 (RequestedUUIDsTooQuickly)
import qualified GenUUID.V1 (run)
import HTTP (Api, server)
import qualified Relation.ManyToMany.InMem (run)
import qualified Relation.ToMany.InMem (run)
import Relation.ToOne.InMem (ExistAction (IgnoreIfExist))
import qualified Relation.ToOne.InMem (run)
import STMWithUnsafeIO (runIOinSTM)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, err404, err500, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (AuthResult (Indefinite), CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import StmContainers.Map as STM.Map (newIO)
import qualified StmContainers.Map as STM (Map)
import StmContainers.Multimap (Multimap)
import qualified StmContainers.Multimap as STM.Multimap (newIO)
import StmContainers.Set as STM.Set (newIO)
import qualified StmContainers.Set as STM (Set)
import Storage.Map.InMem (TableInMem)
import qualified Storage.Map.InMem (run)
import qualified Storage.Set.InMem (run)
import qualified Token.JWT (run)
import qualified Token.JWT.Invalidate.Pure (run)
import qualified UserAction (run)
import qualified VisitorAction (run)
import Servant.Server (err403)

-- | @since 0.2.0.0
--
-- create app by supplying settings and databases(in memory)
mkApp ::
  CookieSettings ->
  JWTSettings ->
  TableInMem UserR ->
  TableInMem ArticleR ->
  TableInMem CommentR ->
  STM.Set Tag ->
  -- | email of user
  STM.Map Email (UserR "id") ->
  -- | article has comment
  Multimap (ArticleR "id") (CommentR "id") ->
  -- | user create article
  Multimap (UserR "id") (ArticleR "id") ->
  -- | article tagged by tag
  Multimap (ArticleR "id") Tag ->
  -- | tag tag article
  Multimap Tag (ArticleR "id") ->
  -- | user follow user
  Multimap (UserR "id") (UserR "id") ->
  -- | user followed by user
  Multimap (UserR "id") (UserR "id") ->
  -- | user favourite article
  Multimap (UserR "id") (ArticleR "id") ->
  -- | article favourited by user
  Multimap (ArticleR "id") (UserR "id") ->
  Application
mkApp cs jwts userDb articleDb commentDb tagDb emailUserIndex db0 db1 db2 db3 db4 db5 db6 db7 =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( runIOinSTM
          . runError @(Forbidden (ArticleR "id"))
          . runError @(NotAuthorized UserR)
          . runError @(NotFound (ArticleR "id"))
          . runError @(NotFound (CommentR "id"))
          . runError @(NotFound (UserR "id"))
          . runError @(NotFound Email)
          . runThrow @(NotFound Tag)
          . runThrow @(AlreadyExists (ArticleR "id"))
          . runThrow @(AlreadyExists (UserR "id"))
          . runThrow @(AlreadyExists (CommentR "id"))
          . runThrow @(AlreadyExists Email)
          . runThrow @(AlreadyLogin UserR)
          . runThrow @(NotLogin UserR)
          . runThrow @ValidationErr
          . runThrow @RequestedUUIDsTooQuickly
          . runThrow @Crypto.JOSE.Error
          . runThrow @Impossible
          . runTrace
          . R.runReader (Indefinite @(UserR "authWithToken"))
          . Current.Reader.run
          . R.runReader jwts
          . R.runReader cs
          . Relation.ToOne.InMem.run @Email @"of" @(UserR "id") @'IgnoreIfExist emailUserIndex
          . Relation.ToMany.InMem.run @(ArticleR "id") @"has" @(CommentR "id") db0
          . Relation.ToMany.InMem.run @(UserR "id") @"create" @(ArticleR "id") db1
          . Relation.ManyToMany.InMem.run @(ArticleR "id") @"taggedBy" @Tag db2 db3
          . Relation.ManyToMany.InMem.run @(UserR "id") @"follow" @(UserR "id") db4 db5
          . Relation.ManyToMany.InMem.run @(UserR "id") @"favorite" @(ArticleR "id") db6 db7
          . Storage.Map.InMem.run @UserR userDb
          . Storage.Map.InMem.run @ArticleR articleDb
          . Storage.Map.InMem.run @CommentR commentDb
          . Storage.Set.InMem.run @Tag tagDb
          . Current.IO.run @Time
          . GenUUID.V1.run
          . ( Token.JWT.run @UserR
                >>> Token.JWT.Invalidate.Pure.run @UserR
            )
          . Authentication.Token.run @UserR
          . VisitorAction.run
          . UserAction.run
          >=> handlerErr err403
          >=> handlerErr err401
          >=> handlerErr err404
          >=> handlerErr err404
          >=> handlerErr err404
          >=> handlerErr err404
          >=> handlerErr err404
          >=> handlerErr err400
          >=> handlerErr err400
          >=> handlerErr err500 -- Comment AlreadyExists use 500
          >=> handlerErr err400
          >=> handlerErr err401 -- ???? FIXME
          >=> handlerErr err401
          >=> handlerErr err400
          >=> handlerErr err500
          >=> handlerErr err400
          >=> handlerErr err500
          >=> \(traces, x) -> print traces >> pure x
      )
      server
  where
    -- NOTE: Helpers for handle errors in form of nested either
    handlerErr' handler = either handler pure
    handlerErr status = handlerErr' (\e -> throwError $ status {errBody = show e})

-- | @since 0.2.0.0
--
-- create app with default settings
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
