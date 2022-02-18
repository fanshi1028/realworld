{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import Data.Generics.Product (getField)
import Data.Storage.Map (IdOf (UserId))
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.Banner (attachConduitBanner, attachProfileBanner)
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Component.List.Article (articleList, profileArticleList)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Component.SignInBox (signInBox)
import InVty.Component.SignUpBox (signUpBox)
import InVty.Component.TagsCollection (mkTagCollecton)
import InVty.Page.Temp (tempPage)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
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
    fixed,
    flex,
    row,
    tile,
  )
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
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m
  ) =>
  ClientEnv ->
  m (Event t LoggedIn)
loggedOutPages clientEnv = mdo
  let -- NOTE: home page /#/
      homePage = Workflow $ do
        (eBanner, eGo) <- attachConduitBanner . row $ do
          rec eGo <- tile flex $ articleList clientEnv Nothing $ Just <$> eTag
              eTag <- tile (fixed 25) $ mkTagCollecton clientEnv
          pure eGo
        pure (never, leftmost [eNavbar, router eGo])
      -- NOTE sign up page /#/register
      signUpPage = Workflow $ do
        rec (eErr, eVErr, eGo) <-
              fst . snd
                <$> splitH3
                  (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) never)
                  (signUpBox clientEnv)
                  blank
        pure (never, router eGo)
      -- NOTE sign in page /#/login
      signInPage = Workflow $ do
        rec (eGo, eErr, eVErr, eRes) <-
              fst . snd
                <$> splitH3
                  (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) never)
                  (signInBox clientEnv)
                  blank
        pure
          ( eRes,
            router eGo
          )
      articlePage slug = tempPage "article page /#/article/:slug" eNavbar -- TEMP FIXME
      -- NOTE: profile page /#/profile/:name
      profilePage uidOrProfile = Workflow $ do
        (eBanner, (eGo, eTagTab)) <- attachProfileBanner $ do
          let user = case uidOrProfile of
                Left (UserId uid) -> uid
                Right prof -> getField @"username" $ getField @"profile" prof
          profileArticleList clientEnv $ pure user
        pure (never, leftmost [eNavbar, router eGo])

      router' (Go p) = case p of
        HomePage -> homePage
        SignInPage -> signInPage
        SignUpPage -> signUpPage
        ArticleContentPage slug -> articlePage slug
        ProfilePage (Just uid) -> profilePage uid
        -- NOTE: below shouldn't be triggered from while logged out.
        ProfilePage Nothing -> err401Page
        EditorPage _ -> err401Page
        SettingsPage -> err401Page

      navBar = router' <<$>> navBarCommonPartWith navBarLoggedOutPart

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err401Page = tempPage "err401 page" eNavbar -- TEMP FIXME
  (eNavbar, eRes) <- splitVRatio 8 navBar $ switchDyn <$> workflow homePage

  pure $ LoggedIn . unOut . getResponse <$> eRes
