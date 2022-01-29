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
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Component.Settings (settingsBox)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    LoggedOut,
    Page (EditArticle, Home, Profile, Settings, SignIn, SignUp),
    noBorderStyle,
    splitH3,
    splitVRatio,
  )
import qualified InVty.Util as Page (Page (Article))
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, fanEither, hold, holdDyn, leftmost, never, switchDyn)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, localTheme, text)
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
    PerformEvent t m
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
      homePage = tempPage "home page /#/" -- TEMP FIXME
      settingsPage = Workflow $ do
        -- NOTE: /#/settings"
        (eLogout', eErr', eRes') <- settingsBox clientEnv dAuth dToken
        pure
          ( leftmost
              [ Left . Left <$> eLogout',
                Left . Right <$> eErr',
                Right . Right <$> eRes'
              ],
            eNavbar
          )
      editorArticlePage mAid = Workflow $ do
        -- NOTE: new article page /#/editor --
        -- NOTE: edit article page /#/editor/:slug --
        (eVErr', eErr', eRes') <- articleEditBox clientEnv mAid dToken
        pure (Left . Right <$> eErr', eNavbar)
      articlePage slug = tempPage "article page /#/article/:slug" -- TEMP FIXME
      profilePage mUid = tempPage "article page /#/profile/:name" -- TEMP FIXME
      -- favouriteUserPage username = tempPage "article page /#/profile/:name/favorites" -- TEMP FIXME
      router' (Go p) = case p of
        Home -> homePage
        EditArticle mAid -> editorArticlePage mAid
        Settings -> settingsPage
        Page.Article slug -> articlePage slug
        Profile mUid -> profilePage mUid
        -- NOTE: Already logged. Just redirect to home page in case it happen? Or 500?
        SignIn -> homePage
        SignUp -> homePage

      navBar = router' <<$>> navBarCommonPartWith navBarLoggedInPart

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err404Page err = tempPage "err404 page" -- TEMP FIXME
      err500Page err = tempPage "err500 page" -- TEMP FIXME
      errorDisplay =
        localTheme (flip withForeColor red <$>) . boxStatic noBorderStyle $
          hold "" (leftmost [show <$> eErr, "" <$ eAuth])
            >>= text
  (eNavbar, (_, (eRes, _))) <- splitVRatio 8 navBar $ splitH3 errorDisplay (switchDyn <$> workflow homePage) blank
  let ( fanEither -> (eLogout, eErr),
        fanEither -> (_, eAuth)
        ) = fanEither eRes
  pure eLogout
