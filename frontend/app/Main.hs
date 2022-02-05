{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module Main where

import Graphics.Vty (Key (KChar, KEsc), Modifier (MCtrl))
import InVty.Scene.LoggedIn (loggedInPages)
import InVty.Scene.LoggedOut (loggedOutPages)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Reflex (never, switchDyn)
import Reflex.Vty (initManager_, keyCombos, mainWidget)
import Reflex.Workflow (Workflow (Workflow), workflow)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https), mkClientEnv)

-- | @since 0.4.0.0
main :: IO ()
main = do
  clientEnv <- mkClientEnv <$> newManager tlsManagerSettings ?? BaseUrl Https "api.realworld.io" 443 ""
  mainWidget $
    initManager_ $ mdo
      let loggedInWF login = Workflow $ do
            e <- loggedInPages clientEnv login
            pure (never, loggedOutWF <$ e)
          loggedOutWF = Workflow $ do
            e <- loggedOutPages clientEnv
            pure (never, loggedInWF <$> e)
      void $ switchDyn <$> workflow loggedOutWF
      (() <$) <$> keyCombos (fromList [(KChar 'c', [MCtrl]), (KEsc, [])])
