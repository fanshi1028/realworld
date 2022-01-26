{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Component.SignInBox (signInBox)
import InVty.Util (Go (Go), LoggedIn (LoggedIn), Page (Home, SignIn, SignUp), splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, ffilter, filterLeft, filterRight, hold, leftmost, never, switchDyn)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, text)
import Reflex.Workflow (Workflow (Workflow), workflow)
import Servant.API (Headers (getResponse))
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
loggedOutPages ::
  ( HasFocusReader t m,
    HasInput t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    HasTheme t m,
    Adjustable t m,
    MonadFix m,
    MonadHold t m,
    HasFocus t m,
    HasLayout t m,
    MonadIO (Performable m),
    PerformEvent t m
  ) =>
  ClientEnv ->
  m (Event t LoggedIn)
loggedOutPages clientEnv = mdo
  let tempPage tag = Workflow $ do
        text $ pure $ "under construction: " <> tag
        pure (never, basicRouting)
      homePage = tempPage "home page /#/" -- TEMP FIXME
      signUpPage = tempPage "sign up page /#/register" -- TEMP FIXME
      signInPage = Workflow $ do
        -- NOTE "sign in page /#/login"
        (eGoSignUp, eErr', eRes') <- signInBox clientEnv
        pure
          ( leftmost [Left <$> eErr', Right <$> eRes'],
            leftmost
              [ signUpPage <$ eGoSignUp,
                basicRouting
              ]
          )
      basicRouting =
        leftmost
          [ homePage <$ ffilter (== Go Home) eGo,
            signInPage <$ ffilter (== Go SignIn) eGo,
            signUpPage <$ ffilter (== Go SignUp) eGo
          ]
      navBar = navBarCommonPartWith navBarLoggedOutPart
      errorGot = hold "no error" (show <$> filterLeft eRes) >>= text
  -- localTheme (flip withForeColor red <$>) . boxStatic thickBoxStyle .
  (eGo, (_, (eRes, _))) <- splitVRatio 8 navBar $ splitH3 errorGot (switchDyn <$> workflow homePage) blank
  pure $ LoggedIn . unOut . getResponse <$> filterRight eRes
