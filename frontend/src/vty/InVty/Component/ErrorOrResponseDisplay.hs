-- | @since 0.4.0.0
module InVty.Component.ErrorOrResponseDisplay where

import Graphics.Vty (green, red, withForeColor)
import InVty.Util (noBorderStyle)
import Reflex (Event, MonadHold, hold, leftmost)
import Reflex.Vty (HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, boxStatic, localTheme, text)

-- | @since 0.4.0.0
errorOrResponseDisplay ::
  ( HasDisplayRegion t m,
    HasImageWriter t m,
    HasInput t m,
    HasFocusReader t m,
    HasTheme t m,
    MonadHold t m
  ) =>
  Event t Text ->
  Event t Text ->
  m ()
errorOrResponseDisplay eErr eRes = do
  bTheme <-
    hold id . leftmost $
      flip withForeColor <<$>> [red <$ eErr, green <$ eRes]
  localTheme (bTheme <*>) . boxStatic noBorderStyle $
    hold "" (leftmost [eErr, eRes]) >>= text
