{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Page.Home where

import Control.Monad.Fix (MonadFix)
import Data.Domain (Domain (User))
import Data.Token.HasToken (TokenOf)
import InVty.Component.Banner (attachConduitBanner, attachProfileBanner)
import InVty.Component.List.Article (articleList)
import InVty.Component.TagsCollection (mkTagCollecton)
import InVty.Util (Go)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PerformEvent, Performable, PostBuild, TriggerEvent, leftmost, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, fixed, flex, row, tile)
import Reflex.Workflow (Workflow (Workflow))
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
homePage ::
  ( MonadIO (Performable m),
    HasInput t m,
    TriggerEvent t m,
    PerformEvent t m,
    PostBuild t m,
    Adjustable t m,
    HasLayout t m,
    HasTheme t m,
    HasFocusReader t m,
    HasImageWriter t m,
    HasFocus t m,
    HasDisplayRegion t m,
    MonadHold t m,
    MonadFix m
  ) =>
  (Event t Go -> Event t (Workflow t m (Event t a))) ->
  ClientEnv ->
  Maybe (Dynamic t (TokenOf 'User)) ->
  Event t (Workflow t m (Event t a)) ->
  Workflow t m (Event t a)
homePage router clientEnv mDToken next = Workflow $ do
  let attachBanner = maybe attachConduitBanner (const attachProfileBanner) mDToken
  (eBanner, eGo) <- attachBanner . row $ do
    rec eGo <- tile flex $ articleList clientEnv mDToken $ Just <$> eTag
        eTag <- tile (fixed 25) $ mkTagCollecton clientEnv
    pure eGo
  pure (never, leftmost [next, router eGo])
