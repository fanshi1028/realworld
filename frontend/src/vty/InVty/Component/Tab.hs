{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Component.Tab where

import Control.Monad.Fix (MonadFix)
import Graphics.Vty (Attr)
import Reflex (Behavior, Event, Reflex (Dynamic, current), difference, gate, hold, leftmost, mergeWith, sample, updated)
import Reflex.Vty
  ( ButtonConfig,
    HasDisplayRegion,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasTheme,
    MonadHold,
    localTheme,
    textButtonStatic,
  )

-- | @since 0.4.0.0
data SelectConfig = SelectConfig
  { unselectThemeModifier :: Attr -> Attr,
    selectThemeModifier :: Attr -> Attr
  }

-- | @since 0.4.0.0
data TabConfig t = TabConfig
  { buttonCfg :: ButtonConfig t,
    selectCfg :: SelectConfig
  }

-- | @since 0.4.0.0
mkThemeModifier ::
  (Reflex t, MonadHold t m) =>
  Event t a ->
  Event t b ->
  (Attr -> Attr) ->
  (Attr -> Attr) ->
  (Attr -> Attr) ->
  m (Behavior t Attr -> Behavior t Attr)
mkThemeModifier eSelected eUnselected initMdf unselectedMdf selectedMdf = do
  bThemeModifier <-
    hold initMdf $
      mergeWith
        (.)
        [ selectedMdf <$ eSelected,
          unselectedMdf <$ eUnselected
        ]
  pure (bThemeModifier <*>)

-- | @since 0.4.0.0
mkTab ::
  ( HasTheme t m,
    HasDisplayRegion t m,
    HasFocusReader t m,
    HasImageWriter t m,
    HasInput t m,
    MonadHold t m,
    MonadFix m,
    Eq a
  ) =>
  TabConfig t ->
  Dynamic t a ->
  Text ->
  a ->
  m (Event t a)
mkTab
  TabConfig
    { buttonCfg,
      selectCfg =
        SelectConfig
          { unselectThemeModifier,
            selectThemeModifier
          }
    }
  dSelected
  key
  page = do
    initSelected <- sample $ current dSelected
    rec eSelectThis <- localTheme (mdf <*>) $ gate ((/= page) <$> current dSelected) <$> textButtonStatic buttonCfg key
        mdf <-
          hold (if initSelected == page then selectThemeModifier else unselectThemeModifier) $
            leftmost
              [ selectThemeModifier <$ eSelectThis,
                unselectThemeModifier
                  <$ gate
                    ((== page) <$> current dSelected)
                    (difference (updated dSelected) eSelectThis)
              ]
    pure $ page <$ eSelectThis
