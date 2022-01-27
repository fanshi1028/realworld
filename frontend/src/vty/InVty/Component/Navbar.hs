-- | @since 0.4.0.0
module InVty.Component.Navbar where

import Graphics.Vty (dim, green, withForeColor, withStyle)
import InVty.Util (Go (Go), Page (EditArticle, Home, Profile, Settings, SignIn, SignUp, WhoAmI), noBorderStyle, splitH2, splitH3, splitHRatio)
import Reflex (Event, Reflex, leftmost)
import Reflex.Vty (ButtonConfig (ButtonConfig), HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, blank, localTheme, textButtonStatic)

-- | @since 0.4.0.0
buttonCfg :: Reflex t => ButtonConfig t
buttonCfg = ButtonConfig (pure noBorderStyle) (pure noBorderStyle)

-- | @since 0.4.0.0
mkButton :: (HasDisplayRegion t m, HasFocusReader t m, HasTheme t m, HasImageWriter t m, HasInput t m) => Text -> Page -> m (Event t Go)
mkButton txt page = (Go page <$) <$> textButtonStatic buttonCfg txt

mkDimButton :: (HasDisplayRegion t m, HasFocusReader t m, HasTheme t m, HasImageWriter t m, HasInput t m) => Text -> Page -> m (Event t Go)
mkDimButton = localTheme ((`withStyle` dim) <$>) <<$>> mkButton

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
      firstSection = (Go Home <$) . fst <$> splitH3 title blank blank
      secondSection = blank
  (eGo1, (_, eGo2)) <- splitH3 firstSection secondSection thirdSection
  pure $ leftmost [eGo1, eGo2]

-- | @since 0.4.0.0
navBarLoggedInPart ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m
  ) =>
  m (Event t Go)
navBarLoggedInPart = do
  ((eGo1, eGo2), (eGo3, eGo4)) <-
    splitH2
      ( splitH2
          (mkButton "Home" Home)
          (mkButton "New article" $ EditArticle Nothing)
      )
      ( splitH2
          (mkButton "Settings" Settings)
          (mkButton "Who am I" WhoAmI)
      )
  pure $ leftmost [eGo1, eGo2, eGo3, eGo4]

-- | @since 0.4.0.0
navBarLoggedOutPart ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m
  ) =>
  m (Event t Go)
navBarLoggedOutPart = do
  (_, (eGo1, (eGo2, eGo3))) <-
    splitH2 blank $
      splitH3
        (mkButton "Home" Home)
        (mkDimButton "Sign in" SignIn)
        (mkDimButton "Sign up" SignUp)
  pure $ leftmost [eGo1, eGo2, eGo3]
