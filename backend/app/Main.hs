{-# LANGUAGE DataKinds #-}

module Main where

import qualified Authentication.Pure (runFalse)
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
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . Transform.run @UserR @"auth" @"authWithToken"
          . Transform.run @UserR @"create" @"all"
          . GenID.Pure.run @UserR
          -- FIXME
          . (usingReaderT undefined . Storage.STM.InMem.run @UserR)
          -- FIXME
          . (usingReaderT undefined . Storage.InMem.run @UserR)
          . Authentication.Pure.runFalse @UserR
          . VisitorAction.Pure.run
          . Tag.Pure.run @[]
          . VisitorAction.Batch.Pure.run @[]
          . R.runReader jwts
          . R.runReader cs
          >=> either
            (\err -> throwError $ err401 {errBody = show err})
            ( either
                (\err -> throwError $ err400 {errBody = show err})
                pure
            )
      )
      server

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  generateKey >>= W.run port . app defaultCookieSettings . defaultJWTSettings
  where
    port = 8080
