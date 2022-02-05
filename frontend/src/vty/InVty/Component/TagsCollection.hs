{-# LANGUAGE FlexibleContexts #-}

-- |
module InVty.Component.TagsCollection where

import Client (getTagsClient)
import Control.Monad.Fix (MonadFix)
import Data.Field.Tag (Tag (Tag, unTag))
import Data.Util.JSON.To (unOut)
import InVty.Util (runRequestE)
import Reflex
  ( Adjustable,
    Event,
    MonadHold (holdDyn),
    PerformEvent,
    Performable,
    PostBuild,
    current,
    getPostBuild,
    leftmost,
    simpleList,
    switchDyn,
    traceEvent,
    (<@),
  )
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
    boxStatic,
    col,
    fixed,
    flex,
    grout,
    row,
    singleBoxStyle,
    stretch,
    textButton,
    tile,
  )
import Servant.Client (ClientEnv)

mkTagCollecton ::
  ( Adjustable t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m),
    PerformEvent t m,
    HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    HasFocus t m,
    HasLayout t m
  ) =>
  ClientEnv ->
  m (Event t Tag)
mkTagCollecton clientEnv = (switchDyn <$>) . (leftmost <<$>>) . tile flex $ do
  (eErr, eRes) <- getPostBuild >>= runRequestE clientEnv . (getTagsClient <$)
  dTags <- holdDyn [] $ leftmost [unOut <$> traceEvent "hi" eRes, [] <$ eErr]
  boxStatic singleBoxStyle . col . simpleList dTags $ \dTag ->
    tile (fixed 3) $
      row $ do
        grout (fixed 1) blank
        t <-
          tile (stretch 20) $
            (current dTag <@)
              <$> textButton
                (ButtonConfig (pure singleBoxStyle) $ pure singleBoxStyle)
                (unTag <$> current dTag)
        grout (fixed 1) blank
        pure t
