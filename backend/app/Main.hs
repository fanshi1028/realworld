module Main where

import Control.Algebra (run)
import HTTP (Api, server)
import qualified Network.Wai.Handler.Warp as W (run)
import Servant (Application, serve)
import Tag.Carrier.Pure (runTagPure)
import VisitorAction.Carrier.Pure (VisitorActionPure(runVisitorActionPure))

app :: Application
app = serve (Proxy @Api) $ server (run . runVisitorActionPure) (run . runTagPure @[])

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  W.run port app
  where
    port = 8080
