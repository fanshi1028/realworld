{-# LANGUAGE DataKinds #-}

module Main where

import Authentication.Pure (SomeNotAuthorized, SomeNotLogin)
import qualified Authentication.Pure (run)
import qualified Authentication.Token.JWT
import qualified Authentication.Token.JWT.Invalidate.Pure
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Crypto.JOSE (Error)
import qualified Current.IO (run)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, Impossible, NotAuthorized, NotFound, ValidationErr)
import Domain.Util.Field (Tag, Time)
import GenUUID.V1 (RequestedUUIDsTooQuickly)
import qualified GenUUID.V1 (run)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified Relation.ManyToMany (run)
import qualified Relation.OneToMany.Pure (run)
import qualified STMWithUnsafeIO (run)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, err404, err500, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import StmContainers.Map as STM.Map (newIO)
import StmContainers.Set as STM.Set (Set, newIO)
import Storage.Map.InMem (TableInMem)
import qualified Storage.Map.InMem (run)
import qualified Storage.Set.InMem (run)
import qualified VisitorAction (run)

app :: CookieSettings -> JWTSettings -> TableInMem UserR -> TableInMem ArticleR -> TableInMem CommentR -> STM.Set.Set Tag -> Application
app cs jwts userDb articleDb commentDb tagDb =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( atomically
          . STMWithUnsafeIO.run
          . runThrow @Impossible
          . runThrow @RequestedUUIDsTooQuickly
          . runThrow @Error
          . runThrow @SomeNotLogin
          . runThrow @SomeNotAuthorized
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . runThrow @(NotFound (UserR "id"))
          . runThrow @(NotFound (ArticleR "id"))
          . runThrow @(NotFound (CommentR "id"))
          . runThrow @(NotFound Tag)
          . runThrow @(AlreadyExists (ArticleR "id"))
          . R.runReader jwts
          . R.runReader cs
          . Relation.OneToMany.Pure.run @(ArticleR "id") @"has" @(CommentR "id") @'True
          . Relation.OneToMany.Pure.run @(UserR "id") @"create" @(ArticleR "id") @'True
          . ( Relation.ManyToMany.run @(UserR "id") @"follow" @(UserR "id")
                >>> Relation.OneToMany.Pure.run @(UserR "id") @"following" @(UserR "id") @'True
                >>> Relation.OneToMany.Pure.run @(UserR "id") @"followedBy" @(UserR "id") @'True
            )
          . ( Relation.ManyToMany.run @(UserR "id") @"favorite" @(ArticleR "id")
                >>> Relation.OneToMany.Pure.run @(ArticleR "id") @"favoritedBy" @(UserR "id") @'True
                >>> Relation.OneToMany.Pure.run @(UserR "id") @"favorite" @(ArticleR "id") @'True
            )
          . (Storage.Map.InMem.run @UserR >>> usingReaderT userDb)
          . (Storage.Map.InMem.run @ArticleR >>> usingReaderT articleDb)
          . (Storage.Map.InMem.run @CommentR >>> usingReaderT commentDb)
          . (usingReaderT tagDb . Storage.Set.InMem.run @Tag)
          . Current.IO.run @Time
          . GenUUID.V1.run
          . Authentication.Token.JWT.Invalidate.Pure.run @UserR
          . Authentication.Token.JWT.run @UserR
          . Authentication.Pure.run @UserR @'False
          . VisitorAction.run
          >=> handlerErr (const $ throwError $ err500 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err500 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err404 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err404 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err404 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err404 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
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
    >>= W.run port
  where
    port = 8080
