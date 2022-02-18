-- | @since 0.4.0.0
module InVty.Component.Banner where

import Control.Monad.Fix (MonadFix)
import Data.Text (center)
import Graphics.Vty (green, reverseVideo, withBackColor, withStyle)
import InVty.Util (noBorderStyle, padText)
import InVty.Util.Split (splitVRatio)
import Reflex (Behavior, Event)
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
    button,
    col,
    fixed,
    flex,
    grout,
    localTheme,
    splitV,
    text,
    tile,
  )

-- | @since 0.4.0.0
data ClickOnBanner = ClickOnBanner

-- | @since 0.4.0.0
attachBanner ::
  ( HasLayout t m,
    HasTheme t m,
    HasInput t m,
    HasFocusReader t m,
    HasImageWriter t m,
    HasFocus t m,
    HasDisplayRegion t m,
    MonadFix m
  ) =>
  m () ->
  m b ->
  m (Event t ClickOnBanner, b)
attachBanner bannerEle contentEle =
  splitVRatio
    4
    ( tile flex . localTheme ((`withBackColor` green) <$>) . col $
        tile
          flex
          ( (ClickOnBanner <$)
              <$> button (ButtonConfig (pure noBorderStyle) (pure noBorderStyle)) bannerEle
          )
          <* grout (fixed 1) blank
    )
    contentEle

-- | @since 0.4.0.0
mkBanner ::
  (HasInput t m, HasDisplayRegion t m, HasFocusReader t m, HasImageWriter t m, HasTheme t m) => m a -> Behavior t Text -> m a
mkBanner mainEle bCaption =
  fst
    <$> splitV
      (pure $ (`div` 6) . (* 5))
      (pure (True, True))
      mainEle
      (localTheme ((`withStyle` reverseVideo) <$>) $ padText center text bCaption)

-- | @since 0.4.0.0
attachConduitBanner ::
  ( MonadFix m,
    HasFocus t m,
    HasDisplayRegion t m,
    HasInput t m,
    HasImageWriter t m,
    HasFocusReader t m,
    HasTheme t m,
    HasLayout t m
  ) =>
  m a ->
  m (Event t ClickOnBanner, a)
attachConduitBanner =
  attachBanner $
    mkBanner
      (snd <$> splitVRatio 5 (text "") (padText center text "Conduit"))
      "A place to share your knowledge."

-- | @since 0.4.0.0
attachProfileBanner ::
  ( MonadFix m,
    HasFocus t m,
    HasDisplayRegion t m,
    HasInput t m,
    HasImageWriter t m,
    HasFocusReader t m,
    HasTheme t m,
    HasLayout t m
  ) =>
  m a ->
  m (Event t ClickOnBanner, a)
attachProfileBanner =
  attachBanner $
    mkBanner
      -- TEMP FIXME
      (snd <$> splitVRatio 5 (text "") (padText center text "TBE profile"))
      "TBE caption"
