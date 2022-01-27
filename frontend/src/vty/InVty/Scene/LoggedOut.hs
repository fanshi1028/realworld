{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import Data.Util.JSON.To (Out (unOut))
import Graphics.Vty (red, withForeColor)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Component.SignInBox (signInBox)
import InVty.Component.SignUpBox (signUpBox)
import InVty.Util (Go (Go), LoggedIn (LoggedIn), Page (Home, SignIn, SignUp), noBorderStyle, splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, fanEither, ffilter, hold, leftmost, never, switchDyn)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, localTheme, text)
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
      signUpPage = Workflow $ do
        -- NOTE "sign up page /#/register"
        (eGoSignIn, eErr', eRes') <- signUpBox clientEnv
        pure
          ( Left <$> eErr',
            leftmost
              [ signInPage <$ eGoSignIn,
                signInPage <$ eRes',
                basicRouting
              ]
          )
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
      errorDisplay =
        localTheme (flip withForeColor red <$>) . boxStatic noBorderStyle $
          hold "" (leftmost [show <$> eErr, "" <$ eOk])
            >>= text
  (eGo, (_, (fanEither -> (eErr, eOk), _))) <- splitVRatio 8 navBar $ splitH3 errorDisplay (switchDyn <$> workflow homePage) blank
  pure $ LoggedIn . unOut . getResponse <$> eOk
