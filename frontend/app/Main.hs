{-# LANGUAGE CPP #-}

-- | @since 0.4.0.0
module Main where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https), mkClientEnv)

#if frontendVty
import InVty.App (app)
#elif frontendJs
import InJs.App (app)
#endif

-- | @since 0.4.0.0
main :: IO ()
main = do
  clientEnv <- mkClientEnv <$> newManager tlsManagerSettings ?? BaseUrl Https "api.realworld.io" 443 ""
#if frontendVty
  app clientEnv
#elif frontendJs
  app clientEnv
#endif
  pure ()
