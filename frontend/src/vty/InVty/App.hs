{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.App where

import Graphics.Vty (Key (KChar, KEsc), Modifier (MCtrl))
import InVty.Scene.LoggedIn (loggedInPages)
import InVty.Scene.LoggedOut (loggedOutPages)
import Reflex.Vty (initManager_, keyCombos, mainWidget, never, switchDyn)
import Reflex.Workflow (Workflow (Workflow), workflow)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
app :: ClientEnv -> IO ()
app clientEnv =
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
