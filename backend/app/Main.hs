{-# LANGUAGE DataKinds #-}

module Main where

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
import Domain.Util.Error (AlreadyExists, AlreadyLogin, Impossible, NotAuthorized, NotFound, NotLogin, ValidationErr)
import Domain.Util.Field (Email, Tag, Time)
import GenUUID.V1 (RequestedUUIDsTooQuickly)
import qualified GenUUID.V1 (run)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified Relation.ManyToMany (run)
import qualified Relation.ToMany.InMem (run)
import Relation.ToOne.InMem (ExistAction (IgnoreIfExist))
import qualified Relation.ToOne.InMem (run)
import qualified STMWithUnsafeIO (run)
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

app ::
  CookieSettings ->
  JWTSettings ->
  TableInMem UserR ->
  TableInMem ArticleR ->
  TableInMem CommentR ->
  STM.Set Tag ->
  STM.Map Email (UserR "id") ->
  Multimap (ArticleR "id") (CommentR "id") ->
  Multimap (UserR "id") (ArticleR "id") ->
  Multimap (ArticleR "id") Tag ->
  Multimap Tag (ArticleR "id") ->
  Multimap (UserR "id") (UserR "id") ->
  Multimap (UserR "id") (UserR "id") ->
  Multimap (ArticleR "id") (UserR "id") ->
  Multimap (UserR "id") (ArticleR "id") ->
  Application
app cs jwts userDb articleDb commentDb tagDb emailUserIndex db0 db1 db2 db3 db4 db5 db6 db7 =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( atomically
          . STMWithUnsafeIO.run
          . runError @(NotAuthorized UserR)
          . runError @(NotFound (ArticleR "id"))
          . runError @(NotFound (CommentR "id"))
          . runError @(NotFound (UserR "id"))
          . runThrow @Impossible
          . runThrow @RequestedUUIDsTooQuickly
          . runThrow @Crypto.JOSE.Error
          . runThrow @(NotLogin UserR)
          . runThrow @(NotAuthorized UserR)
          . runError @(NotFound Email)
          . runThrow @ValidationErr
          . runThrow @(NotFound Tag)
          . runThrow @(AlreadyExists (ArticleR "id"))
          . runThrow @(AlreadyExists Email)
          . runThrow @(AlreadyExists (UserR "id"))
          . runThrow @(AlreadyLogin UserR)
          . runTrace
          . R.runReader (Indefinite @(UserR "authWithToken"))
          . Current.Reader.run
          . R.runReader jwts
          . R.runReader cs
          . Relation.ToOne.InMem.run @Email @"of" @(UserR "id") @'IgnoreIfExist emailUserIndex
          . Relation.ToMany.InMem.run @(ArticleR "id") @"has" @(CommentR "id") db0
          . Relation.ToMany.InMem.run @(UserR "id") @"create" @(ArticleR "id") db1
          . ( Relation.ManyToMany.run @(ArticleR "id") @"taggedBy" @Tag
                >>> Relation.ToMany.InMem.run @(ArticleR "id") @"taggedBy" @Tag db2
                >>> Relation.ToMany.InMem.run @Tag @"tagging" @(ArticleR "id") db3
            )
          . ( Relation.ManyToMany.run @(UserR "id") @"follow" @(UserR "id")
                >>> Relation.ToMany.InMem.run @(UserR "id") @"following" @(UserR "id") db4
                >>> Relation.ToMany.InMem.run @(UserR "id") @"followedBy" @(UserR "id") db5
            )
          . ( Relation.ManyToMany.run @(UserR "id") @"favorite" @(ArticleR "id")
                >>> Relation.ToMany.InMem.run @(ArticleR "id") @"favoritedBy" @(UserR "id") db6
                >>> Relation.ToMany.InMem.run @(UserR "id") @"favorite" @(ArticleR "id") db7
            )
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
          >=> handlerErr (\e -> throwError $ err500 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err500 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err401 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err401 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err401 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err404 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err404 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err404 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err404 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err404 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> handlerErr (\e -> throwError $ err400 {errBody = show e})
          >=> \(traces, x) -> print traces >> pure x
      )
      server
  where
    -- NOTE: Helpers for handle errors in form of nested either
    handlerErr handler = either handler pure

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  app defaultCookieSettings . defaultJWTSettings
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
    >>= W.run port
  where
    port = 8080
