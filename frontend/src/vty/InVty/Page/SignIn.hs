{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Page.SignIn (signInPage) where

import Control.Monad.Fix (MonadFix)
import Data.Domain.User (UserAuthWithToken)
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Component.SignInBox (signInBox)
import InVty.Util (Go)
import InVty.Util.Split (splitH3)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, leftmost, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank)
import Reflex.Workflow (Workflow (Workflow))
import Servant.API (getResponse)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
signInPage ::
  ( HasDisplayRegion t m,
    HasInput t m,
    HasImageWriter t m,
    HasFocusReader t m,
    HasTheme t m,
    Adjustable t m,
    HasFocus t m,
    HasLayout t m,
    MonadIO (Performable m),
    PerformEvent t m,
    MonadHold t m,
    MonadFix m
  ) =>
  (Event t Go -> Event t (Workflow t m (Event t UserAuthWithToken))) ->
  ClientEnv ->
  Workflow t m (Event t UserAuthWithToken)
signInPage router clientEnv = Workflow $ do
  rec (eGo, eErr, eVErr, eRes) <-
        fst . snd
          <$> splitH3
            (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) never)
            (signInBox clientEnv)
            blank
  pure
    ( unOut . getResponse <$> eRes,
      router eGo
    )
