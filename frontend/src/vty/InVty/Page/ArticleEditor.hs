{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Page.ArticleEditor where

import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasToken (TokenOf)
import Data.Domain (Domain (User))
import InVty.Component.ArticleEditBox (articleEditBox)
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Util (ArticleIdOrContent, Go)
import InVty.Util.Split (splitH3)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PerformEvent, Performable, leftmost, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank)
import Reflex.Workflow (Workflow)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
articleEditorPage ::
  ( HasDisplayRegion t m,
    HasInput t m,
    HasImageWriter t m,
    HasFocusReader t m,
    HasTheme t m,
    Adjustable t m,
    HasFocus t m,
    HasLayout t m,
    PerformEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  ( Event t Go ->
    Event t (Workflow t m (Event t a))
  ) ->
  ClientEnv ->
  Dynamic t (TokenOf 'User) ->
  Maybe ArticleIdOrContent ->
  m (Event t a, Event t (Workflow t m (Event t a)))
articleEditorPage router clientEnv dToken mAid = do
  rec (eVErr, eErr, eRes) <-
        fst . snd
          <$> splitH3
            (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) $ show <$> eRes)
            (articleEditBox clientEnv mAid dToken)
            blank
  pure (never, router never)
