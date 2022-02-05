{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedOut where

import Control.Monad.Fix (MonadFix)
import Data.Util.JSON.To (Out (unOut))
import Graphics.Vty (green, red, reverseVideo, withBackColor, withForeColor, withStyle)
import InVty.Component.ArticleList (articleList)
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedOutPart)
import InVty.Component.SignInBox (signInBox)
import InVty.Component.SignUpBox (signUpBox)
import InVty.Util
  ( Go (Go),
    LoggedIn (LoggedIn),
    Page (ArticleContentPage, EditorPage, HomePage, ProfilePage, SettingsPage, SignInPage, SignUpPage),
    centerText,
    noBorderStyle,
    splitH3,
    splitVRatio,
  )
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, PostBuild, fanEither, hold, leftmost, never, switchDyn)
import Reflex.Vty
  ( HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    blank,
    box,
    boxStatic,
    flex,
    localTheme,
    splitH,
    splitV,
    text,
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
    PostBuild t m
  ) =>
  ClientEnv ->
  m (Event t LoggedIn)
loggedOutPages clientEnv = mdo
  let tempPage tag = Workflow $ do
        text $ pure $ "under construction: " <> tag
        pure (never, eNavbar)

      -- NOTE: homePage = tempPage "home page /#/" -- TEMP FIXME
      homePage = Workflow $ do
        (_, (eGo, ())) <-
          attachBanner
            ( splitV
                (pure $ (`div` 6) . (* 5))
                (pure (True, True))
                (splitVRatio 5 (text "") (centerText text "Conduit"))
                $ localTheme ((`withStyle` reverseVideo) <$>) $ centerText text "A place to share your knowledge."
                --  $ centerText text "A place to share your knowledge."
            )
            ( splitH
                (pure $ (`div` 3) . (* 2))
                (pure (True, True))
                (articleList clientEnv Nothing $ pure Nothing)
                blank
            )
        pure (never, leftmost [eNavbar, router eGo])
      -- NOTE "sign up page /#/register"
      signUpPage = Workflow $ do
        (eErr', eVErr', eGo) <- fst . snd <$> splitH3 errorDisplay (signUpBox clientEnv) blank
        pure (leftmost [Left . Left <$> eVErr', Left . Right <$> eErr'], router eGo)
      -- NOTE "sign in page /#/login"
      signInPage = Workflow $ do
        (eGo, eErr', eVErr', eRes') <- fst . snd <$> splitH3 errorDisplay (signInBox clientEnv) blank
        pure
          ( leftmost
              [ Left . Left <$> eVErr',
                Left . Right <$> eErr',
                Right . Right <$> eRes'
              ],
            router eGo
          )
      articlePage slug = tempPage "article page /#/article/:slug" -- TEMP FIXME
      profilePage uid = tempPage "profile page /#/profile/:name" -- TEMP FIXME
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

      err401Page = tempPage "err401 page" -- TEMP FIXME
      errorDisplay =
        localTheme (flip withForeColor red <$>) . boxStatic noBorderStyle $
          hold "" (leftmost [show <$> eVErr, show <$> eErr, "" <$ eOk, "" <$ eNavbar])
            >>= text
      attachBanner bannerEle contentEle =
        splitVRatio
          5
          ( tile flex . localTheme ((`withBackColor` green) <$>) $
              box (pure noBorderStyle) bannerEle
          )
          contentEle

  (eNavbar, eRes) <- splitVRatio 8 navBar $ switchDyn <$> workflow homePage

  let ( fanEither ->
          ( fanEither -> (eVErr, eErr),
            fanEither -> (_, eOk)
            )
        ) = eRes
  pure $ LoggedIn . unOut . getResponse <$> eOk
