{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedIn where

import Control.Monad.Fix (MonadFix)
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Data.Generics.Product (getField)
import Data.Storage.Map (IdOf (UserId))
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Page.ArticleEditor (articleEditorPage)
import InVty.Page.Home (homePage)
import InVty.Page.Profile (profilePage)
import InVty.Page.Settings (settingsPage)
import InVty.Page.Temp (tempPage)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    LoggedOut,
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
    current,
    fanEither,
    holdDyn,
    leftmost,
    sample,
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
loggedInPages ::
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
  LoggedIn ->
  m (Event t LoggedOut)
loggedInPages clientEnv (LoggedIn (UserAuthWithToken auth token)) = mdo
  dAuth <- holdDyn auth $ eAuth <&> \(UserAuthWithToken auth' _) -> auth'
  dToken <- holdDyn token $ eAuth <&> \(UserAuthWithToken _ t) -> t

  let homePage' = homePage router clientEnv (Just dToken)
      router' (Go p) = Workflow $ case p of
        HomePage -> homePage' -- NOTE: /#/
        EditorPage mAidOrContent -> articleEditorPage router clientEnv dToken mAidOrContent -- NOTE: /#/editor, /#/editor/:slug
        SettingsPage -> settingsPage router clientEnv dAuth dToken -- NOTE: /#/settings
        ArticleContentPage slug -> tempPage router "article page /#/article/:slug" -- TEMP FIXME
        -- NOTE: who am i FIXME what route?
        ProfilePage Nothing -> do
          uid <- UserId . getField @"username" <$> sample (current dAuth)
          profilePage router clientEnv (Just dAuth) $ Left uid
        -- NOTE: /#/profile/:name
        ProfilePage (Just uidOrProfile) -> profilePage router clientEnv (Just dAuth) uidOrProfile
        -- NOTE: FIXME Already logged. Just redirect to home page in case it happen? Or 500?
        SignInPage -> homePage'
        SignUpPage -> homePage'

      router eGo = leftmost [router' <$> eGo, eNavbar]

  (eNavbar, fanEither -> (eLogout, eAuth)) <-
    splitVRatio 8 (router' <<$>> navBarCommonPartWith navBarLoggedInPart) $
      switchDyn <$> workflow (Workflow homePage')
  pure eLogout
