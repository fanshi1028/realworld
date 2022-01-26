-- | @since 0.4.0.0
module InVty.Component.Navbar where

import Graphics.Vty (dim, green, withForeColor, withStyle)
import InVty.Util (Go (Go), Page (Home, NewArticle, SignIn, SignUp), noBorderStyle, splitH3, splitHRatio)
import Reflex (Event, Reflex, leftmost)
import Reflex.Vty (ButtonConfig (ButtonConfig), HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, blank, localTheme, textButtonStatic)

-- | @since 0.4.0.0
buttonCfg :: Reflex t => ButtonConfig t
buttonCfg = ButtonConfig (pure noBorderStyle) (pure noBorderStyle)

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
  let homeButton = textButtonStatic buttonCfg "Home"
      newArticle = textButtonStatic buttonCfg "New article"
      settings = textButtonStatic buttonCfg "Settings"
      whoAmI = textButtonStatic buttonCfg "Profile"
  (_, ((eGoHome, eGoNewArticle), (eGoSignIn, eGoSignUp))) <- splitHRatio 2 blank (splitHRatio 2 (splitHRatio 2 homeButton newArticle) $ splitHRatio 2 settings whoAmI)
  pure $ leftmost [Go Home <$ eGoHome, Go NewArticle <$ eGoNewArticle, Go SignIn <$ eGoSignIn, Go SignUp <$ eGoSignUp]

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
  let homeButton = textButtonStatic buttonCfg "Home"
      signInButton = localTheme ((`withStyle` dim) <$>) $ textButtonStatic buttonCfg "Sign in"
      signUpButton = localTheme ((`withStyle` dim) <$>) $ textButtonStatic buttonCfg "Sign up"
  (_, (eGoHome, (eGoSignIn, eGoSignUp))) <- splitHRatio 2 blank (splitH3 homeButton signInButton signUpButton)
  pure $ leftmost [Go Home <$ eGoHome, Go SignIn <$ eGoSignIn, Go SignUp <$ eGoSignUp]
