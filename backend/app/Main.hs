module Main where

import InMem.App (newApp)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  newApp >>= run port
  where
    port = 8080
