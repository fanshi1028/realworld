module Main where

import HTTP (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  run port app
  where
    port = 8080
