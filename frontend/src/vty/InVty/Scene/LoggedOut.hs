{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Util (Go (Go), LoggedIn, Page (Home, SignIn, SignUp), splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, ffilter, filterLeft, filterRight, hold, leftmost, never, switchDyn)
import Reflex.Vty (HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, blank, text)
import Reflex.Workflow (Workflow (Workflow), workflow)
import Servant.Client (ClientEnv, ClientError)

-- | @since 0.4.0.0
loggedOutPages ::
  ( HasFocusReader t m,
    HasInput t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    HasTheme t m,
    Adjustable t m,
    MonadFix m,
    MonadHold t m
  ) =>
  ClientEnv ->
  m (Event t LoggedIn)
loggedOutPages clientEnv = mdo
  let tempPage tag = Workflow $ do
        text $ pure $ "under construction: " <> tag
        pure (never, basicRouting)
      homePage = tempPage "home page /#/" -- TEMP FIXME
      signUpPage = tempPage "sign up page /#/register" -- TEMP FIXME
      signInPage = tempPage "sign in page /#/login" -- TEMP FIXME
      basicRouting =
        leftmost
          [ homePage <$ ffilter (== Go Home) eGo,
            signInPage <$ ffilter (== Go SignIn) eGo,
            signUpPage <$ ffilter (== Go SignUp) eGo
          ]
      navBar = navBarCommonPartWith navBarLoggedOutPart
      errorGot = hold "no error" (show @_ @ClientError <$> filterLeft eRes) >>= text
  -- localTheme (flip withForeColor red <$>) . boxStatic thickBoxStyle .
  (eGo, (_, (eRes, _))) <- splitVRatio 8 navBar $ splitH3 errorGot (switchDyn <$> workflow homePage) blank
  pure $ filterRight eRes
