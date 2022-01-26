{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Scene.LoggedIn where

import Control.Monad.Fix (MonadFix)
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import InVty.Component.Navbar (navBarCommonPartWith, navBarLoggedInPart)
import InVty.Component.Settings (settingsBox)
import InVty.Util (Go (Go), LoggedIn (LoggedIn), LoggedOut, Page (Home, NewArticle, Profile, Settings), splitH3, splitVRatio)
import qualified InVty.Util as Page (Page (Article))
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, ffilter, filterLeft, filterRight, fmapMaybe, hold, holdDyn, leftmost, never, switchDyn)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, text)
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
  -- FIXME
  dAuth <- holdDyn auth never
  -- FIXME
  dToken <- holdDyn token never
  let tempPage tag = Workflow $ do
        text $ pure $ "under construction: " <> tag
        pure (never, basicRouting)
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
            basicRouting
          )
      editorCreatePage = tempPage "editor page /#/editor" -- TEMP FIXME
      -- editorEditPage = tempPage "editor page /#/editor/:slug" -- TEMP FIXME
      articlePage slug = tempPage "article page /#/article/:slug" -- TEMP FIXME
      profilePage username = tempPage "article page /#/profile/:name" -- TEMP FIXME
      -- favouriteUserPage username = tempPage "article page /#/profile/:name/favorites" -- TEMP FIXME
      basicRouting =
        leftmost
          [ homePage <$ ffilter (== Go Home) eGo,
            settingsPage <$ ffilter (== Go Settings) eGo,
            editorCreatePage <$ ffilter (== Go NewArticle) eGo,
            settingsPage <$ ffilter (== Go Settings) eGo,
            ( \case
                Go (Profile name) -> Just $ profilePage name
                _ -> Nothing
            )
              `fmapMaybe` eGo,
            ( \case
                Go (Page.Article slug) -> Just $ articlePage slug
                _ -> Nothing
            )
              `fmapMaybe` eGo
          ]
      navBar = navBarCommonPartWith navBarLoggedInPart
      errorGot = hold "no error" (show <$> filterRight (filterLeft eRes)) >>= text
  -- localTheme (flip withForeColor red <$>) . boxStatic thickBoxStyle .
  (eGo, (_, (eRes, _))) <- splitVRatio 8 navBar $ splitH3 errorGot (switchDyn <$> workflow homePage) blank
  pure $ filterLeft (filterLeft eRes)
