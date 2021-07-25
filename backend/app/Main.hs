{-# LANGUAGE DataKinds #-}

module Main where

import Authentication.Pure (SomeNotAuthorized, SomeNotLogin)
import qualified Authentication.Pure (run)
import qualified Authentication.Token.JWT
import qualified Authentication.Token.JWT.Invalidate.Pure
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Crypto.JOSE (Error)
import qualified CurrentTime.IO
import Domain.Article (ArticleR)
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, NotAuthorized, NotFound, ValidationErr)
import qualified GenID.Pure (run)
import qualified GenID.UUID.Pure
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, err404, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import StmContainers.Map (newIO)
import Storage.InMem (TableInMem)
import qualified Storage.InMem (run)
import qualified Storage.STM.InMem (run)
import qualified Tag.Pure (run)
import qualified Transform (run)
import Util.Orphan ()
import qualified VisitorAction.Batch.Pure (run)
import qualified VisitorAction.Pure (run)

app :: CookieSettings -> JWTSettings -> TableInMem UserR -> TableInMem ArticleR -> TableInMem CommentR -> Application
app cs jwts userDb articleDb commentDb =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( runM
          . runThrow @Error
          . runThrow @SomeNotLogin
          . runThrow @SomeNotAuthorized
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . runThrow @(NotFound (UserR "id"))
          . runThrow @(NotFound (ArticleR "id"))
          . runThrow @(AlreadyExists (ArticleR "id"))
          . R.runReader jwts
          . R.runReader cs
          . (usingReaderT userDb . Storage.STM.InMem.run @UserR)
          . (usingReaderT articleDb . Storage.STM.InMem.run @ArticleR)
          . (usingReaderT commentDb . Storage.STM.InMem.run @CommentR)
          . Transform.run @ArticleR @"all" @"withAuthorProfile"
          . Transform.run @ArticleR @"create" @"all"
          . Transform.run @UserR @"auth" @"authWithToken"
          . Transform.run @UserR @"create" @"all"
          . CurrentTime.IO.run
          . GenID.Pure.run @UserR
          . GenID.Pure.run @ArticleR
          . GenID.UUID.Pure.run @CommentR
          . Storage.InMem.run @UserR
          . Authentication.Token.JWT.Invalidate.Pure.run @UserR
          . Authentication.Token.JWT.run @UserR
          . Authentication.Pure.run @UserR @'False
          . VisitorAction.Pure.run
          . Tag.Pure.run @[]
          . VisitorAction.Batch.Pure.run @[]
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
          >=> handlerErr (const $ throwError $ err401 {errBody = "fuck"})
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
