{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- | @since 0.4.0.0
module InVty.Component.Settings where

import Client (updateUserClient)
import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (AuthOf (..))
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (..))
import Data.Field.Bio (Bio (Bio))
import Data.Field.Email (Email (Email))
import Data.Field.Image (Image (Image))
import Data.Field.Password (mkPassword)
import Data.Field.Username (Username (Username))
import Data.Generic.HKD (Build (build), Construct (construct))
import Data.Storage.Map (Patch, UpdateOf)
import Data.Text.Zipper (fromText)
import Data.Token.HasToken (TokenOf (..))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out)
import GHC.Records (HasField (getField))
import Graphics.Vty (bold, green, red, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (inputWithPlaceHolder)
import InVty.Util (LoggedOut (LoggedOut), centerText, noBorderStyle, splitH3, splitVRatio)
import Reflex
  ( Adjustable,
    Event,
    MonadHold,
    PerformEvent (Performable, performEvent),
    Reflex (Dynamic),
    current,
    fanEither,
    sample,
    (<@),
  )
import Reflex.Vty
  ( HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    blank,
    boxStatic,
    button,
    def,
    doubleBoxStyle,
    localTheme,
    multilineTextInput,
    singleBoxStyle,
    text,
    textButtonStatic,
    textInput,
    _buttonConfig_focusStyle,
  )
import Servant.Client.Streaming (ClientEnv, ClientError, withClientM)
import Validation (Validation (Success))

-- | @since 0.4.0.0
settingsBox ::
  ( Reflex t,
    MonadHold t m,
    MonadFix m,
    HasInput t m,
    HasFocusReader t m,
    HasTheme t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    Adjustable t m,
    HasFocus t m,
    HasLayout t m,
    MonadIO (Performable m),
    PerformEvent t m
  ) =>
  ClientEnv ->
  Dynamic t (AuthOf 'User) ->
  Dynamic t (TokenOf 'User) ->
  m (Event t LoggedOut, Event t ClientError, Event t (Out UserAuthWithToken))
settingsBox clientEnv dAuth dToken = mdo
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      inputAreaWithPlaceHolder = inputWithPlaceHolder multilineTextInput singleBoxStyle doubleBoxStyle

      title = localTheme ((`withStyle` bold) <$>) $ centerText text "Settings"
      avatorInput = Image <<$>> inputBoxWithPlaceHolder "https://api.realworld.io/images/smiley-cyrus.jpeg"
      nameInput =
        Username <<$>> do
          Username name' <- sample $ getField @"username" <$> current dAuth
          inputBoxWithPlaceHolder $ fromText name'
      firstSection = snd <$> splitVRatio 2 (splitVRatio 2 blank title) (splitVRatio 2 avatorInput nameInput)

      sencodSection = Bio <<$>> inputAreaWithPlaceHolder "Short bio about you"

      emailInput =
        Email <<$>> do
          Email email' <- sample $ getField @"email" <$> current dAuth
          inputBoxWithPlaceHolder $ fromText email'
      pwInput = mkPassword <<$>> inputBoxWithPlaceHolder "New Password"
      updateButton =
        snd . snd
          <$> splitH3
            blank
            blank
            ( button def {_buttonConfig_focusStyle = pure noBorderStyle} . localTheme ((`withBackColor` green) <$>) $
                boxStatic noBorderStyle $ centerText text "Update Settings"
            )
      logoutButton =
        localTheme ((`withForeColor` red) <$>) $
          fst <$> splitH3 (textButtonStatic def "Or click here to logout.") blank blank
      thirdSection =
        splitVRatio 3 (splitVRatio 2 emailInput pwInput) $ splitVRatio 2 updateButton logoutButton

  ((dAvatorInput, dNameInput), (dBioInput, ((dEmailInput, dPwInput), (eUpdate, eLogout)))) <-
    splitVRatio 4 firstSection $ splitVRatio 2 sencodSection thirdSection

  let dPayload =
        In . Success
          <$> construct
            ( build @(Patch (UpdateOf 'User))
                (pure . pure <$> dEmailInput)
                (pure . pure <$> dPwInput)
                (pure . pure <$> dNameInput)
                (pure . pure <$> dBioInput)
                (pure . pure <$> dAvatorInput)
            )
      eRequest = current (updateUserClient <$> dToken <*> dPayload) <@ eUpdate
      eRunRequest = liftIO <$> (flip withClientM clientEnv <$> eRequest ?? pure)
  (eErr, eRes) <- fanEither <$> performEvent eRunRequest
  pure (LoggedOut <$ eLogout, eErr, eRes)
