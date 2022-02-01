{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Component.Tab where

import Control.Monad.Fix (MonadFix)
import Graphics.Vty (Attr)
import Reflex
  ( Adjustable,
    Event,
    PostBuild,
    Reflex (current),
    fromUniqDynamic,
    holdDyn,
    selectViewListWithKey,
    splitE,
    uniqDynamic,
    (<@),
  )
import Reflex.Vty
  ( ButtonConfig,
    HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    MonadHold,
    flex,
    localTheme,
    row,
    textButtonStatic,
    tile,
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
mkTab ::
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
  TabConfig t ->
  Text ->
  Map Text tab ->
  m (Event t tab)
mkTab
  TabConfig
    { buttonCfg,
      selectCfg =
        SelectConfig
          { unselectThemeModifier,
            selectThemeModifier
          }
    }
  initSelectKey
  tabs = do
    rec dFocusedTab <- fromUniqDynamic . uniqDynamic <$> holdDyn initSelectKey eKey
        (splitE -> (eKey, eTab)) <- row $
          selectViewListWithKey dFocusedTab (pure tabs) $ \k dTab dSelected ->
            tile flex $ do
              let bThemeMdf = bool unselectThemeModifier selectThemeModifier <$> current dSelected
              (current dTab <@) <$> localTheme (bThemeMdf <*>) (textButtonStatic buttonCfg k)
    pure eTab
