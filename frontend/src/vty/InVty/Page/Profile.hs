{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Page.Profile where

import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (User))
import Data.Generics.Product (getField)
import Data.Storage.Map (IdOf (UserId))
import InVty.Component.Banner (attachProfileBanner)
import InVty.Component.List.Article (profileArticleList)
import InVty.Util (Go, UserIdOrProfile)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PerformEvent, Performable, PostBuild, TriggerEvent, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme)
import Reflex.Workflow (Workflow)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
profilePage ::
  ( HasDisplayRegion t m,
    HasLayout t m,
    HasTheme t m,
    HasFocusReader t m,
    HasImageWriter t m,
    HasInput t m,
    HasFocus t m,
    Adjustable t m,
    PostBuild t m,
    PerformEvent t m,
    MonadIO (Performable m),
    TriggerEvent t m,
    MonadHold t m,
    MonadFix m
  ) =>
  ( Event t Go ->
    Event t (Workflow t m (Event t a))
  ) ->
  ClientEnv ->
  Maybe (Dynamic t (AuthOf 'User)) ->
  UserIdOrProfile ->
  m (Event t a, Event t (Workflow t m (Event t a)))
profilePage router clientEnv mDAuth uidOrProfile = do
  (eBanner, (eGo, eTagTab)) <- attachProfileBanner $ do
    let dUser = case uidOrProfile of
          Left (UserId user) -> pure user
          Right prof -> pure . getField @"username" $ getField @"profile" prof
    profileArticleList clientEnv dUser
  pure (never, router eGo)
