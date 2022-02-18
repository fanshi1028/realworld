{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Page.SignUp where

import Control.Monad.Fix (MonadFix)
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Component.SignUpBox (signUpBox)
import InVty.Util (Go)
import InVty.Util.Split (splitH3)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, leftmost, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank)
import Reflex.Workflow (Workflow)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
signUpPage ::
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
  (Event t Go -> Event t (Workflow t m (Event t a))) ->
  ClientEnv ->
  m (Event t a, Event t (Workflow t m (Event t a)))
signUpPage router clientEnv = do
  rec (eErr, eVErr, eGo) <-
        fst . snd
          <$> splitH3
            (errorOrResponseDisplay (leftmost [show <$> eVErr, show <$> eErr]) never)
            (signUpBox clientEnv)
            blank
  pure (never, router eGo)
