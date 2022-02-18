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
import InVty.Component.ArticleEditBox (articleEditBox)
import InVty.Component.Banner (attachProfileBanner)
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Component.List.Article (profileArticleList)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Component.Settings (settingsBox)
import InVty.Page.Home (homePage)
import InVty.Page.Temp (tempPage)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    LoggedOut,
    Page (ArticleContentPage, EditorPage, HomePage, ProfilePage, SettingsPage, SignInPage, SignUpPage),
  )
import InVty.Util.Split (splitH3, splitVRatio)
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
    blank,
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
      homePage' = homePage router clientEnv (Just dToken) eNavbar
      -- NOTE: setting page /#/settings
      settingsPage = Workflow $ do
        rec (eLogout', eErr, eRes) <-
              fst . snd
                <$> splitH3
                  (errorOrResponseDisplay (show <$> eErr) $ show <$> eRes)
                  (settingsBox clientEnv dAuth dToken)
                  blank
        pure
          ( leftmost
              [ Left <$> eLogout',
                Right <$> eRes
              ],
            eNavbar
          )
      -- NOTE: new article page /#/editor
      -- NOTE: edit article page /#/editor/:slug
      editorArticlePage mAid = Workflow $ do
        rec (eVErr, eErr, eRes) <-
              fst . snd
                <$> splitH3
                  (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) $ show <$> eRes)
                  (articleEditBox clientEnv mAid dToken)
                  blank
        pure (never, eNavbar)
      articlePage slug = tempPage "article page /#/article/:slug" eNavbar -- TEMP FIXME
      -- NOTE: profile page /#/profile/:name
      profilePage mUidOrProfile = Workflow $ do
        (eBanner, (eGo, eTagTab)) <- attachProfileBanner $ do
          let dUser = case mUidOrProfile of
                Nothing -> getField @"username" <$> dAuth
                Just (Left (UserId user)) -> pure user
                Just (Right prof) -> pure . getField @"username" $ getField @"profile" prof
          profileArticleList clientEnv dUser
        pure (never, leftmost [eNavbar, router eGo])
      router' (Go p) = case p of
        HomePage -> homePage'
        EditorPage mAid -> editorArticlePage mAid
        SettingsPage -> settingsPage
        ArticleContentPage slug -> articlePage slug
        ProfilePage mUidOrProfie -> profilePage mUidOrProfie
        -- NOTE: Already logged. Just redirect to home page in case it happen? Or 500?
        SignInPage -> homePage'
        SignUpPage -> homePage'

      navBar = router' <<$>> navBarCommonPartWith navBarLoggedInPart

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err404Page err = tempPage "err404 page" eNavbar -- TEMP FIXME
      err500Page err = tempPage "err500 page" eNavbar -- TEMP FIXME
  (eNavbar, fanEither -> (eLogout, eAuth)) <- splitVRatio 8 navBar $ switchDyn <$> workflow homePage'
  pure eLogout
