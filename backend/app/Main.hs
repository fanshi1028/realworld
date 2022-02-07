{-# LANGUAGE CPP #-}

module Main where

import Network.Wai.Handler.Warp
  ( Port,
#if backendInMem || backendRel8
    run,
#endif
  )

#if backendInMem
import InMem.App (newApp)
#elif backendRel8
import InRel8.App (newApp)
#endif

main :: IO ()
main = do
  putStrLn $ "server running at port: " <> show @_ @Port port
#if backendInMem
  InMem.App.newApp >>= run port
#elif backendRel8
  InRel8.App.newApp "" >>= run port
#endif
  putStrLn "Just kidding! Server is not running, probably wrong cpp-options"
  where
    port = 8080
