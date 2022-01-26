{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Component.InputBox where

import Control.Monad.Fix (MonadFix)
import Data.Text.Zipper (TextZipper, home)
import Reflex
  ( Adjustable,
    Dynamic,
    Event,
    MonadHold,
    Reflex (Behavior),
    current,
    fforMaybe,
    fromUniqDynamic,
    never,
    uniqDynamic,
    updated,
  )
import Reflex.Vty
  ( BoxStyle,
    HasDisplayRegion,
    HasFocus,
    HasFocusReader (focus),
    HasImageWriter,
    HasInput,
    HasTheme,
    TextInput,
    TextInputConfig (_textInputConfig_modify),
    box,
    def,
    ffilter,
    flex,
    tile,
    _textInputConfig_initialValue,
    _textInput_value,
  )
import Reflex.Vty.Widget.Layout (HasLayout)
import Reflex.Workflow (Workflow (Workflow), workflow)

-- | @since 0.4.0.0
mkInput ::
  ( HasDisplayRegion t m,
    HasImageWriter t m,
    HasInput t m,
    HasFocusReader t m,
    HasTheme t m,
    Reflex t
  ) =>
  (TextInputConfig t -> m (TextInput t)) ->
  Behavior t BoxStyle ->
  TextZipper ->
  Event t (TextZipper -> TextZipper) ->
  m (TextInput t)
mkInput inputMaker bStyle initial eModify =
  box bStyle . inputMaker $
    def
      { _textInputConfig_initialValue = initial,
        _textInputConfig_modify = eModify
      }

-- | @since 0.4.0.0
inputWithPlaceHolder ::
  ( HasInput t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    HasFocusReader t m,
    HasTheme t m,
    MonadHold t m,
    MonadFix m,
    Adjustable t m,
    HasFocus t m,
    HasLayout t m
  ) =>
  (TextInputConfig t -> m (TextInput t)) ->
  BoxStyle ->
  BoxStyle ->
  TextZipper ->
  m (Dynamic t Text)
inputWithPlaceHolder inputMaker nfStyle fStyle placeHolder = tile flex $ do
  df <- focus
  let eFocus = ffilter id $ updated df
      bStyle = bool nfStyle fStyle <$> current df
      wf1 = Workflow $ do
        -- NOTE: holdUniqDyn??
        -- NOTE: is (fromUniqDynamic . uniqDynamic) faster??
        dText@(fmap toString . updated . fromUniqDynamic . uniqDynamic -> eString) <- _textInput_value <$> mkInput inputMaker bStyle (home placeHolder) (home <$ eFocus)
        pure . (dText,) . fforMaybe eString $ \case
          (ch : _) -> Just $ wf2 ch
          _ -> Nothing
      wf2 ch = Workflow $ do
        dText@(updated -> eText) <- _textInput_value <$> mkInput inputMaker bStyle (fromString [ch]) never
        pure (dText, wf1 <$ ffilter (null . toString) eText)
  join <$> workflow wf1