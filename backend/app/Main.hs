module Main where

import Control.Carrier.Lift (runM)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, hoistServer, serve)
import Tag.Carrier.Pure (runTagPure)
import VisitorAction.Carrier.Pure (runVisitorActionPure)

app :: Application
app =
  serve (Proxy @Api) $
    hoistServer
      (Proxy @Api)
      ( runM
          . runVisitorActionPure
          . runTagPure @[]
      )
      server

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  W.run port app
  where
    port = 8080
