{-# LANGUAGE DataKinds #-}

module Main where

import qualified Authentication.Pure (runFalse)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized, ValidationErr)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, Context (EmptyContext, (:.)), ServerError (errBody), err400, err401, hoistServerWithContext, serveWithContext, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import qualified StmContainers.Map as STM
import Storage.InMem (TableInMem')
import qualified Tag.Pure (run)
import qualified VisitorAction.Batch.Pure (run)
import qualified VisitorAction.Pure (run)

app :: TableInMem' UserR "token" "id" -> CookieSettings -> JWTSettings -> Application
app table cs jwts =
  serveWithContext (Proxy @Api) (cs :. jwts :. table :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings, TableInMem' UserR "token" "id"])
      ( runM
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . Authentication.Pure.runFalse @UserR
          . VisitorAction.Pure.run
          . Tag.Pure.run @[]
          . VisitorAction.Batch.Pure.run @[]
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
  table <- STM.newIO
  generateKey >>= W.run port . app table defaultCookieSettings . defaultJWTSettings
  where
    port = 8080
