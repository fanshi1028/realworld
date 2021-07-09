module Main where

import Control.Carrier.Lift (runM)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, hoistServer, serve)
import qualified VisitorAction.Pure (run)
import qualified VisitorAction.Batch.Pure (run)
import qualified Tag.Pure (run)

app :: Application
app =
  serve (Proxy @Api) $
    hoistServer
      (Proxy @Api)
      ( runM
          . VisitorAction.Pure.run
          . Tag.Pure.run @[]
          . VisitorAction.Batch.Pure.run @[]
      )
      server

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  W.run port app
  where
    port = 8080
