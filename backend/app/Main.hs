module Main where

import App.InMem (newApp)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show port
  newApp >>= run port
  where
    port = 8080
