{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Page.Settings where

import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken)
import Data.Token.HasToken (TokenOf)
import InVty.Component.ErrorOrResponseDisplay (errorOrResponseDisplay)
import InVty.Component.Settings (settingsBox)
import InVty.Util (LoggedOut)
import InVty.Util.Split (splitH3)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PerformEvent, Performable, leftmost, never)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank)
import Reflex.Workflow (Workflow)
import Servant.Client (ClientEnv)

-- | @since 0.4.0.0
settingsPage ::
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
  ( Event t a ->
    Event t (Workflow t m (Event t (Either LoggedOut UserAuthWithToken)))
  ) ->
  ClientEnv ->
  Dynamic t (AuthOf 'User) ->
  Dynamic t (TokenOf 'User) ->
  m
    ( Event t (Either LoggedOut UserAuthWithToken),
      Event t (Workflow t m (Event t (Either LoggedOut UserAuthWithToken)))
    )
settingsPage router clientEnv dAuth dToken = do
  rec (eLogout, eErr, eRes) <-
        fst . snd
          <$> splitH3
            (errorOrResponseDisplay (show <$> eErr) $ show <$> eRes)
            (settingsBox clientEnv dAuth dToken)
            blank
  pure
    ( leftmost
        [ Left <$> eLogout,
          Right <$> eRes
        ],
      router never
    )
