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
import InVty.Util (Go (Go), LoggedIn (LoggedIn), Page (EditArticle, Home, Profile, Settings, SignIn, SignUp), noBorderStyle, splitH3, splitVRatio)
import qualified InVty.Util as Page (Page (Article))
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, fanEither, hold, leftmost, never, switchDyn)
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
        pure (never, eNavbar)
      homePage = tempPage "home page /#/" -- TEMP FIXME
      signUpPage = Workflow $ do
        -- NOTE "sign up page /#/register"
        (eErr', eVErr', eGo) <- signUpBox clientEnv
        pure (leftmost [Left . Left <$> eVErr', Left . Right <$> eErr'], router eGo)
      signInPage = Workflow $ do
        -- NOTE "sign in page /#/login"
        (eGo, eErr', eVErr', eRes') <- signInBox clientEnv
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
        Home -> homePage
        SignIn -> signInPage
        SignUp -> signUpPage
        Page.Article slug -> articlePage slug
        Profile (Just uid) -> profilePage uid
        -- NOTE: below shouldn't be triggered from while logged out.
        Profile Nothing -> err401Page
        EditArticle _ -> err401Page
        Settings -> err401Page

      navBar = router' <<$>> navBarCommonPartWith navBarLoggedOutPart

      router eGo = leftmost [router' <$> eGo, eNavbar]

      err401Page = tempPage "err401 page" -- TEMP FIXME
      errorDisplay =
        localTheme (flip withForeColor red <$>) . boxStatic noBorderStyle $
          hold "" (leftmost [show <$> eVErr, show <$> eErr, "" <$ eOk, "" <$ eNavbar])
            >>= text
  (eNavbar, (_, (eRes, _))) <-
    splitVRatio 8 navBar $ splitH3 errorDisplay (switchDyn <$> workflow homePage) blank
  let ( fanEither ->
          ( fanEither -> (eVErr, eErr),
            fanEither -> (_, eOk)
            )
        ) = eRes
  pure $ LoggedIn . unOut . getResponse <$> eOk
