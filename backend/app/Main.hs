module Main where

import qualified Authorization.Pure (runFalse)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized, ValidationErr)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, ServerError (errBody), err400, err401, hoistServer, serve, throwError)
import qualified Tag.Pure (run)
import qualified VisitorAction.Batch.Pure (run)
import qualified VisitorAction.Pure (run)

app :: Application
app =
  serve (Proxy @Api) $
    hoistServer
      (Proxy @Api)
      ( runM
          . runThrow @(NotAuthorized UserR)
          . runThrow @ValidationErr
          . Authorization.Pure.runFalse @UserR
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
  W.run port app
  where
    port = 8080
