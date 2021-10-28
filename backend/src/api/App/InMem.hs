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

import Authentication (LoginOf)
import qualified Authentication.Token (run)
import Control.Carrier.Error.Either (runError)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Returning (runTrace)
import Control.Exception.Safe (catch)
import qualified Crypto.JOSE (Error)
import qualified Current.IO (run)
import qualified Current.Reader (run)
import Domain (Domain (Article, Comment, User))
import Field.Email (Email)
import Field.Tag (Tag)
import Field.Time (Time)
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
import Servant.Server (err403)
import StmContainers.Map as STM.Map (newIO)
import qualified StmContainers.Map as STM (Map)
import StmContainers.Multimap (Multimap)
import qualified StmContainers.Multimap as STM.Multimap (newIO)
import StmContainers.Set as STM.Set (newIO)
import qualified StmContainers.Set as STM (Set)
import Storage.Map (IdOf)
import Storage.Map.InMem (TableInMem)
import qualified Storage.Map.InMem (run)
import qualified Storage.Set.InMem (run)
import Token (TokenOf (..))
import qualified Token.JWT (run)
import qualified Token.JWT.Invalidate.Pure (run)
import Domain.User (UserR)
import qualified UserAction (run)
import Util.Error (AlreadyExists, AlreadyLogin, Forbidden, Impossible, NotAuthorized, NotFound, NotLogin, ValidationErr)
import qualified VisitorAction (run)

-- | @since 0.2.0.0
--
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
  Application
mkApp cs jwts userDb articleDb commentDb tagDb emailUserIndex db0 db1 db2 db3 db4 db5 db6 db7 =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( ( runIOinSTM
            . runError @(Forbidden (IdOf 'Article))
            . runError @(NotAuthorized (IdOf 'User))
            . runError @(NotAuthorized ())
            . runError @(NotAuthorized (TokenOf 'User))
            . runError @(NotFound (IdOf 'Article))
            . runError @(NotFound (IdOf 'Comment))
            . runError @(NotFound (IdOf 'User))
            . runError @(NotFound Email)
            . runThrow @(NotFound Tag)
            . runThrow @(AlreadyExists (IdOf 'Article))
            . runThrow @(AlreadyExists (IdOf 'User))
            . runThrow @(AlreadyExists (IdOf 'Comment))
            . runThrow @(AlreadyExists Email)
            . runThrow @(AlreadyLogin (LoginOf 'User))
            . runThrow @(NotLogin ())
            . runThrow @ValidationErr
            . runThrow @RequestedUUIDsTooQuickly
            . runThrow @Crypto.JOSE.Error
            . runThrow @Impossible
            . runTrace
            . R.runReader (Indefinite @(UserR "authWithToken"))
            . Current.Reader.run
            . R.runReader jwts
            . R.runReader cs
            . Relation.ToOne.InMem.run @Email @"of" @(IdOf 'User) @'IgnoreIfExist emailUserIndex
            . Relation.ToMany.InMem.run @(IdOf 'Article) @"has" @(IdOf 'Comment) db0
            . Relation.ToMany.InMem.run @(IdOf 'User) @"create" @(IdOf 'Article) db1
            . Relation.ManyToMany.InMem.run @(IdOf 'Article) @"taggedBy" @Tag db2 db3
            . Relation.ManyToMany.InMem.run @(IdOf 'User) @"follow" @(IdOf 'User) db4 db5
            . Relation.ManyToMany.InMem.run @(IdOf 'User) @"favorite" @(IdOf 'Article) db6 db7
            . Storage.Map.InMem.run @'User userDb
            . Storage.Map.InMem.run @'Article articleDb
            . Storage.Map.InMem.run @'Comment commentDb
            . Storage.Set.InMem.run @Tag tagDb
            . Current.IO.run @Time
            . GenUUID.V1.run
            . ( Token.JWT.run @'User
                  >>> Token.JWT.Invalidate.Pure.run @(TokenOf 'User)
              )
            . Authentication.Token.run @'User
            . VisitorAction.run
            . UserAction.run
            >=> handlerErr err403
            >=> handlerErr err401
            >=> handlerErr err401
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
            >=> pure . snd
        )
          >>> atomically
          >>> (`catch` throwError)
      )
      server
  where
    -- NOTE: Helpers for handle errors in form of nested either
    handlerErr' handler = either handler pure
    -- handlerErr status = handlerErr' (\e -> throwError $ status {errBody = show e})
    handlerErr status = handlerErr' (\e -> throwSTM $ status {errBody = show e})

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
