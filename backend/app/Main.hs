{-# LANGUAGE DataKinds #-}

module Main where

import Authentication.Pure (SomeNotAuthorized, SomeNotLogin)
import qualified Authentication.Pure (run)
import qualified Authentication.Token.JWT
import qualified Authentication.Token.JWT.Invalidate.Pure
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Crypto.JOSE (Error)
import qualified CurrentTime.IO (run)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, NotAuthorized, NotFound, ValidationErr)
import GenUUID.V1 (RequestedUUIDsTooQuickly)
import qualified GenUUID.V1 (run)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified Relation.OneToMany.Pure (run)
import qualified STMWithUnsafeIO (run)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, err404, err500, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import StmContainers.Map (newIO)
import Storage.Map.InMem (TableInMem)
import qualified Storage.Map.InMem (run)
import qualified Tag.Pure (run)
import qualified VisitorAction (run)

app :: CookieSettings -> JWTSettings -> TableInMem UserR -> TableInMem ArticleR -> TableInMem CommentR -> Application
app cs jwts userDb articleDb commentDb =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( atomically
          . STMWithUnsafeIO.run
          . runThrow @RequestedUUIDsTooQuickly
          . runThrow @Error
          . runThrow @SomeNotLogin
          . runThrow @SomeNotAuthorized
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . runThrow @(NotFound (UserR "id"))
          . runThrow @(NotFound (ArticleR "id"))
          . runThrow @(NotFound (CommentR "id"))
          . runThrow @(AlreadyExists (ArticleR "id"))
          . R.runReader jwts
          . R.runReader cs
          . Relation.OneToMany.Pure.run @(ArticleR "id") @"has" @(CommentR "id") @'True
          . (usingReaderT userDb . Storage.Map.InMem.run @UserR)
          . (usingReaderT articleDb . Storage.Map.InMem.run @ArticleR)
          . (usingReaderT commentDb . Storage.Map.InMem.run @CommentR)
          . CurrentTime.IO.run
          . GenUUID.V1.run
          . Authentication.Token.JWT.Invalidate.Pure.run @UserR
          . Authentication.Token.JWT.run @UserR
          . Authentication.Pure.run @UserR @'False
          . Tag.Pure.run @[]
          . VisitorAction.run
          >=> handlerErr (const $ throwError $ err500 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
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
    <*> newIO
    <*> newIO
    <*> newIO
    >>= W.run port
  where
    port = 8080
