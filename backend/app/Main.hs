{-# LANGUAGE DataKinds #-}

module Main where

import Authentication.Pure (SomeNotLogin)
import qualified Authentication.Pure (run)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Domain.User (UserR (..))
import Domain.Util.Error (NotAuthorized, ValidationErr)
import qualified GenID.Pure (run)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import qualified Storage.InMem (run)
import qualified Storage.STM.InMem (run)
import qualified Tag.Pure (run)
import qualified Transform (run)
import qualified VisitorAction.Batch.Pure (run)
import qualified VisitorAction.Pure (run)

app :: CookieSettings -> JWTSettings -> Application
app cs jwts =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( runM
          . runThrow @SomeNotLogin
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . Transform.run @UserR @"auth" @"authWithToken"
          . Transform.run @UserR @"create" @"all"
          . GenID.Pure.run @UserR
          -- FIXME
          . (usingReaderT undefined . Storage.STM.InMem.run @UserR)
          -- FIXME
          . (usingReaderT undefined . Storage.InMem.run @UserR)
          . Authentication.Pure.run @UserR @False
          . VisitorAction.Pure.run
          . Tag.Pure.run @[]
          . VisitorAction.Batch.Pure.run @[]
          . R.runReader jwts
          . R.runReader cs
          >=> handlerErr (\err -> throwError $ err400 {errBody = "fuck"})
          >=> handlerErr (\err -> throwError $ err401 {errBody = "fuck"})
          >=> handlerErr (\err -> throwError $ err400 {errBody = "fuck"})
      )
      server
  where
    -- NOTE: Helpers for handle errors in form of nested either
    handlerErr handler = either handler pure

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  generateKey >>= W.run port . app defaultCookieSettings . defaultJWTSettings
  where
    port = 8080
