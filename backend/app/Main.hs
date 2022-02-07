{-# LANGUAGE CPP #-}

module Main where

#if backendinmem
import InMem.App (newApp)
#else if backendrel8
import InRel8.App (newApp)
#endif

import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $
    "server running at port: " <> show port
#if backendinmem
  InMem.App.newApp
#else if backendrel8
  InRel8.App.newApp ""
#endif
      >>= run port
  where
    port = 8080
