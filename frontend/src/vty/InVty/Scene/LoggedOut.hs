{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Page.Home (homePage)
import InVty.Page.Profile (profilePage)
import InVty.Page.SignIn (signInPage)
import InVty.Page.SignUp (signUpPage)
import InVty.Page.Temp (tempPage)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    Page (ArticleContentPage, EditorPage, HomePage, ProfilePage, SettingsPage, SignInPage, SignUpPage),
  )
import InVty.Util.Split (splitVRatio)
import Reflex
  ( Adjustable,
    Event,
    MonadHold,
    PerformEvent,
    Performable,
    PostBuild,
    TriggerEvent,
    leftmost,
    switchDyn,
  )
import Reflex.Vty
  ( HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
  )
import Reflex.Workflow (Workflow (Workflow), workflow)
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
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m
  ) =>
  ClientEnv ->
  m (Event t LoggedIn)
loggedOutPages clientEnv = mdo
  let homePage' = homePage router clientEnv Nothing

      err401Page = tempPage router "err401 page" -- TEMP FIXME
      router' (Go p) = Workflow $ case p of
        HomePage -> homePage' -- NOTE: /#/
        SignInPage -> signInPage router clientEnv -- NOTE /#/login
        SignUpPage -> signUpPage router clientEnv -- NOTE /#/register
        ArticleContentPage slug -> tempPage router "article page /#/article/:slug" -- TEMP FIXME
        ProfilePage (Just uidOrProf) -> profilePage router clientEnv Nothing uidOrProf -- NOTE: /#/profile/:name
        -- NOTE: below shouldn't be triggered from while logged out.
        ProfilePage Nothing -> err401Page
        EditorPage _ -> err401Page
        SettingsPage -> err401Page

      router eGo = leftmost [router' <$> eGo, eNavbar]

  (eNavbar, eAuth) <-
    splitVRatio 8 (router' <<$>> navBarCommonPartWith navBarLoggedOutPart) $
      switchDyn <$> workflow (Workflow homePage')

  pure $ LoggedIn <$> eAuth
