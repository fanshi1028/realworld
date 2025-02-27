-- | @since 0.4.0.0
module InVty.Component.Navbar where

import Control.Monad.Fix (MonadFix)
import Graphics.Vty (defAttr, dim, green, withForeColor, withStyle)
import InVty.Component.Tab (SelectConfig (SelectConfig), TabConfig (TabConfig), Tabable (toTabKey), mkTab)
import InVty.Util (Go (Go), Page (EditorPage, HomePage, ProfilePage, SettingsPage, SignInPage, SignUpPage), noBorderStyle)
import InVty.Util.Split (splitH3)
import Reflex (Adjustable, Event, MonadHold, PostBuild, Reflex, leftmost, updated)
import Reflex.Vty
  ( ButtonConfig (ButtonConfig),
    HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    blank,
    flex,
    localTheme,
    textButtonStatic,
  )

-- | @since 0.4.0.0
buttonCfg :: Reflex t => ButtonConfig t
buttonCfg = ButtonConfig (pure noBorderStyle) (pure noBorderStyle)

-- | @since 0.4.0.0
tabCfg :: Reflex t => TabConfig t
tabCfg = TabConfig buttonCfg $ SelectConfig (`withStyle` dim) (const defAttr)

-- | @since 0.4.0.0
navBarCommonPartWith ::
  ( HasTheme t m,
    HasDisplayRegion t m,
    HasFocusReader t m,
    HasImageWriter t m,
    HasInput t m
  ) =>
  m (Event t Go) ->
  m (Event t Go)
navBarCommonPartWith thirdSection = do
  let title = localTheme ((`withForeColor` green) <$>) $ textButtonStatic buttonCfg "conduit"
      firstSection = (Go HomePage <$) . fst <$> splitH3 title blank blank
      secondSection = blank
  (eGo1, (_, eGo2)) <- splitH3 firstSection secondSection thirdSection
  pure $ leftmost [eGo1, eGo2]

-- | @since 0.4.0.0
navBarLoggedInPart ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    MonadFix m,
    MonadHold t m,
    Adjustable t m,
    PostBuild t m,
    HasFocus t m,
    HasLayout t m
  ) =>
  m (Event t Go)
navBarLoggedInPart =
  Go <<$>> do
    updated <$> do
      mkTab tabCfg flex HomePage . pure . fromList $
        (toTabKey &&& id)
          <$> [ HomePage,
                EditorPage Nothing,
                SettingsPage,
                ProfilePage Nothing
              ]

-- | @since 0.4.0.0
navBarLoggedOutPart ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    MonadHold t m,
    MonadFix m,
    Adjustable t m,
    PostBuild t m,
    HasFocus t m,
    HasLayout t m
  ) =>
  m (Event t Go)
navBarLoggedOutPart =
  Go <<$>> do
    updated <$> do
      mkTab tabCfg flex HomePage . pure . fromList $
        (toTabKey &&& id)
          <$> [ HomePage,
                SignInPage,
                SignUpPage
              ]
