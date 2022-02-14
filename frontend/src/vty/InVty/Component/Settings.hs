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
import Data.Text (center)
import Data.Text.Zipper (fromText)
import Data.Token.HasToken (TokenOf (..))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (unOut))
import GHC.Records (HasField (getField))
import Graphics.Vty (bold, green, red, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (PlaceHolderMode (Edit, Replace), inputWithPlaceHolder)
import InVty.Util (LoggedOut (LoggedOut), noBorderStyle, padText, runRequestE, splitH3, splitV3, splitVRatio)
import Reflex
  ( Adjustable,
    Event,
    MonadHold,
    PerformEvent (Performable),
    Reflex (Dynamic),
    current,
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
import Servant.Client.Streaming (ClientEnv, ClientError)
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
  m (Event t LoggedOut, Event t ClientError, Event t UserAuthWithToken)
settingsBox clientEnv dAuth dToken = mdo
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      inputAreaWithPlaceHolder = inputWithPlaceHolder multilineTextInput singleBoxStyle doubleBoxStyle

      title = localTheme ((`withStyle` bold) <$>) $ padText center text "Settings"
      avatorInput = fmap Image <<$>> inputBoxWithPlaceHolder Edit "https://api.realworld.io/images/smiley-cyrus.jpeg"
      nameInput =
        fmap Username <<$>> do
          Username name' <- sample $ getField @"username" <$> current dAuth
          inputBoxWithPlaceHolder Edit $ fromText name'
      firstSection = snd <$> splitV3 title avatorInput nameInput

      sencodSection =
        fmap Bio <<$>> do
          Bio bio' <- sample $ getField @"bio" <$> current dAuth
          inputAreaWithPlaceHolder Edit $ fromText bio'

      emailInput =
        fmap Email <<$>> do
          Email email' <- sample $ getField @"email" <$> current dAuth
          inputBoxWithPlaceHolder Edit $ fromText email'
      pwInput = fmap mkPassword <<$>> inputBoxWithPlaceHolder Replace "New Password"
      updateButton =
        snd . snd
          <$> splitH3
            blank
            blank
            ( button def {_buttonConfig_focusStyle = pure noBorderStyle} . localTheme ((`withBackColor` green) <$>) $
                boxStatic noBorderStyle $ padText center text "Update Settings"
            )
      logoutButton =
        (LoggedOut <$)
          <$> localTheme ((`withForeColor` red) <$>) (fst <$> splitH3 (textButtonStatic def "Or click here to logout.") blank blank)
      thirdSection =
        splitVRatio 5 emailInput $ splitVRatio 4 pwInput $ splitVRatio 2 updateButton logoutButton

  ((dMAvatorInput, dMNameInput), (dMBioInput, (dMEmailInput, (dMPwInput, (eUpdate, eLogout))))) <-
    splitVRatio 5 firstSection $ splitVRatio 2 sencodSection thirdSection

  let dPayload =
        In . Success
          <$> construct
            ( build @(Patch (UpdateOf 'User))
                (pure <<$>> dMEmailInput)
                (pure <<$>> dMPwInput)
                (pure <<$>> dMNameInput)
                (pure <<$>> dMBioInput)
                (pure <<$>> dMAvatorInput)
            )
      eRequest = current (updateUserClient <$> dToken <*> dPayload) <@ eUpdate
  (eErr, eRes) <- runRequestE clientEnv eRequest
  pure (eLogout, eErr, unOut <$> eRes)
