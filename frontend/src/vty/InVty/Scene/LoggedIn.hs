{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedIn where

import Control.Monad.Fix (MonadFix)
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Graphics.Vty (red, withForeColor)
import InVty.Component.ArticleEditBox (articleEditBox)
import InVty.Component.ArticleList (articleList)
import InVty.Component.Banner (attachProfileBanner)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Component.Settings (settingsBox)
import InVty.Component.TagsCollection (mkTagCollecton)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    LoggedOut,
    Page (ArticleContentPage, EditorPage, HomePage, ProfilePage, SettingsPage, SignInPage, SignUpPage),
    noBorderStyle,
    splitH3,
    splitVRatio,
  )
import Reflex
  ( Adjustable,
    Event,
    MonadHold,
    PerformEvent,
    Performable,
    PostBuild,
    TriggerEvent,
    fanEither,
    hold,
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
    boxStatic,
    fixed,
    flex,
    localTheme,
    row,
    text,
    tile,
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

  let tempPage tag = Workflow $ do
        text $ pure $ "under construction: " <> tag
        pure (never, eNavbar)
      -- NOTE: home page /#/
      homePage = Workflow $ do
        (eBanner, eGo) <- attachProfileBanner . row $ do
          rec eGo <- tile flex $ articleList clientEnv (Just dToken) $ Just <$> eTag
              eTag <- tile (fixed 25) $ mkTagCollecton clientEnv
          pure eGo
        pure (never, leftmost [eNavbar, router eGo])
      -- NOTE: /#/settings"
      settingsPage = Workflow $ do
        (_, ((eLogout', eErr', eRes'), _)) <- splitH3 errorDisplay (settingsBox clientEnv dAuth dToken) blank
        pure
          ( leftmost
              [ Left . Right <$> eErr',
                Right . Left <$> eLogout',
                Right . Right <$> eRes'
              ],
            eNavbar
          )
      -- NOTE: new article page /#/editor --
      -- NOTE: edit article page /#/editor/:slug --
      editorArticlePage mAid = Workflow $ do
        (_, ((eVErr', eErr', eRes'), _)) <- splitH3 errorDisplay (articleEditBox clientEnv mAid dToken) blank
        pure (leftmost [Left . Left <$> eVErr', Left . Right <$> eErr'], eNavbar)
      articlePage slug = tempPage "article page /#/article/:slug" -- TEMP FIXME
      profilePage mUid = tempPage "article page /#/profile/:name" -- TEMP FIXME
      -- favouriteUserPage username = tempPage "article page /#/profile/:name/favorites" -- TEMP FIXME
      router' (Go p) = case p of
        HomePage -> homePage
        EditorPage mAid -> editorArticlePage mAid
        SettingsPage -> settingsPage
        ArticleContentPage slug -> articlePage slug
        ProfilePage mUid -> profilePage mUid
        -- NOTE: Already logged. Just redirect to home page in case it happen? Or 500?
        SignInPage -> homePage
        SignUpPage -> homePage

      navBar = router' <<$>> navBarCommonPartWith navBarLoggedInPart

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err404Page err = tempPage "err404 page" -- TEMP FIXME
      err500Page err = tempPage "err500 page" -- TEMP FIXME
      errorDisplay =
        localTheme (flip withForeColor red <$>) . boxStatic noBorderStyle $
          hold "" (leftmost [show <$> eErr, show <$> eVErr, "" <$ eAuth, "" <$ eNavbar])
            >>= text
  (eNavbar, eRes) <- splitVRatio 8 navBar $ switchDyn <$> workflow homePage
  let ( fanEither -> (eVErr, eErr),
        fanEither -> (eLogout, eAuth)
        ) = fanEither eRes
  pure eLogout
