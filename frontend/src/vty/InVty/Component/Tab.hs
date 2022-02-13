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
    Reflex (Dynamic, current),
    fromUniqDynamic,
    holdDyn,
    leftmost,
    never,
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
    localTheme,
    row,
    textButton,
    tile,
  )
import qualified Reflex.Vty.Widget.Layout as Layout (Constraint)

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
class Tabable tab where
  -- | @since 0.4.0.0
  toTabKey :: tab -> Integer

  -- | @since 0.4.0.0
  toTabName :: tab -> Text

-- | @since 0.4.0.0
mkTab' ::
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
    HasLayout t m,
    Tabable tab
  ) =>
  TabConfig t ->
  Dynamic t Layout.Constraint ->
  tab ->
  Dynamic t (Map Integer tab) ->
  Event t tab ->
  m (Dynamic t tab)
mkTab'
  TabConfig
    { buttonCfg,
      selectCfg =
        SelectConfig
          { unselectThemeModifier,
            selectThemeModifier
          }
    }
  dConstraint
  initSelectTab
  dTabs
  eSelectTab = do
    rec dTab <- holdDyn initSelectTab (leftmost [eSelectTab, eTab])
        (splitE -> (_, eTab)) <- row $
          selectViewListWithKey (fromUniqDynamic . uniqDynamic $ toTabKey <$> dTab) dTabs $ \_key dTab' dSelected ->
            tile dConstraint $ do
              let bThemeMdf = bool unselectThemeModifier selectThemeModifier <$> current dSelected
              (current dTab' <@) <$> localTheme (bThemeMdf <*>) (textButton buttonCfg (toTabName <$> current dTab'))
    pure dTab

-- | @since 0.4.0.0
mkTab ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    Adjustable t m,
    PostBuild t m,
    HasFocus t m,
    HasLayout t m,
    MonadHold t m,
    MonadFix m,
    Tabable tab
  ) =>
  TabConfig t ->
  Dynamic t Layout.Constraint ->
  tab ->
  Dynamic t (Map Integer tab) ->
  m (Dynamic t tab)
mkTab tabCfg dConstraint initSelectTab dTabs = mkTab' tabCfg dConstraint initSelectTab dTabs never
