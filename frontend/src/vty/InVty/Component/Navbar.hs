{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Component.Navbar where

import Control.Monad.Fix (MonadFix)
import Graphics.Vty (defAttr, dim, green, withForeColor, withStyle)
import InVty.Component.Tab (SelectConfig (SelectConfig), TabConfig (TabConfig), mkTab)
import InVty.Util (Go (Go), Page (EditArticle, Home, Profile, Settings, SignIn, SignUp), noBorderStyle, splitH2, splitH3)
import Reflex (Event, MonadHold, Reflex, leftmost)
import Reflex.Vty
  ( ButtonConfig (ButtonConfig),
    HasDisplayRegion,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasTheme,
    blank,
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
    HasInput t m,
    MonadFix m,
    MonadHold t m
  ) =>
  m (Event t Go)
navBarLoggedInPart =
  Go <<$>> do
    rec let eGo = leftmost [eGo1, eGo2, eGo3, eGo4]
            mkTab' = mkTab tabCfg eGo "Home"
        ((eGo1, eGo2), (eGo3, eGo4)) <-
          splitH2
            ( splitH2
                (mkTab' "Home" Home)
                (mkTab' "New article" $ EditArticle Nothing)
            )
            ( splitH2
                (mkTab' "Settings" Settings)
                (mkTab' "Who am I" $ Profile Nothing)
            )
    pure eGo

-- | @since 0.4.0.0
navBarLoggedOutPart ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m (Event t Go)
navBarLoggedOutPart =
  Go <<$>> do
    rec let eGo = leftmost [eGo1, eGo2, eGo3]
            mkTab' = mkTab tabCfg eGo "Home"
        (_, (eGo1, (eGo2, eGo3))) <- do
          splitH2 blank $ splitH3 (mkTab' "Home" Home) (mkTab' "Sign in" SignIn) (mkTab' "Sign up" SignUp)
    pure eGo
