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
import InVty.Component.Banner (attachProfileBanner)
import InVty.Component.List.Article (profileArticleList)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Page.ArticleEditor (articleEditorPage)
import InVty.Page.Home (homePage)
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
    fanEither,
    holdDyn,
    leftmost,
    never,
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

  let -- NOTE: home page /#/
      homePage' = homePage router clientEnv (Just dToken)
      articlePage slug = tempPage "article page /#/article/:slug" eNavbar -- TEMP FIXME
      -- NOTE: profile page /#/profile/:name
      profilePage mUidOrProfile = do
        (eBanner, (eGo, eTagTab)) <- attachProfileBanner $ do
          let dUser = case mUidOrProfile of
                Nothing -> getField @"username" <$> dAuth
                Just (Left (UserId user)) -> pure user
                Just (Right prof) -> pure . getField @"username" $ getField @"profile" prof
          profileArticleList clientEnv dUser
        pure (never, leftmost [eNavbar, router eGo])
      router' (Go p) = Workflow $ case p of
        HomePage -> homePage'
        EditorPage mAidOrContent -> articleEditorPage router clientEnv dToken mAidOrContent -- NOTE: /#/editor, /#/editor/:slug
        SettingsPage -> settingsPage router clientEnv dAuth dToken -- NOTE: /#/settings
        ArticleContentPage slug -> articlePage slug
        ProfilePage mUidOrProfie -> profilePage mUidOrProfie
        -- NOTE: Already logged. Just redirect to home page in case it happen? Or 500?
        SignInPage -> homePage'
        SignUpPage -> homePage'

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err404Page err = tempPage "err404 page" eNavbar -- TEMP FIXME
      err500Page err = tempPage "err500 page" eNavbar -- TEMP FIXME
  (eNavbar, fanEither -> (eLogout, eAuth)) <-
    splitVRatio 8 (router' <<$>> navBarCommonPartWith navBarLoggedInPart) $
      switchDyn <$> workflow (Workflow homePage')
  pure eLogout
