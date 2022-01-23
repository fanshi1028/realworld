{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

-- | @since 0.4.0.0
module InVty.Component.Settings where

import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (User))
import Data.Field.Bio (Bio (Bio))
import Data.Field.Email (Email (Email))
import Data.Field.Image (Image (Image))
import Data.Field.Password (mkPassword)
import Data.Field.Username (Username (Username))
import Data.Generic.HKD (Build (build), Construct (construct))
import Data.Storage.Map (Patch, UpdateOf)
import Data.Text.Zipper (fromText)
import Effect.UserAction (UserActionE (UpdateUser))
import Graphics.Vty (bold, green, red, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (inputWithPlaceHolder)
import InVty.Util (centerText, noBorderStyle, splitH3, splitV3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, Reflex (Dynamic), current, sample, tag)
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
    HasLayout t m
  ) =>
  Dynamic t Username ->
  Dynamic t Email ->
  m (Event t (UserActionE n (AuthOf 'User)), Event t ())
settingsBox dName dEmail = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      inputAreaWithPlaceHolder = inputWithPlaceHolder multilineTextInput singleBoxStyle doubleBoxStyle

      title = localTheme ((`withStyle` bold) <$>) $ centerText text "Settings"
      avatorInput = Image <<$>> inputBoxWithPlaceHolder "https://api.realworld.io/images/smiley-cyrus.jpeg"
      nameInput =
        Username <<$>> do
          Username name' <- sample $ current dName
          inputBoxWithPlaceHolder $ fromText name'
      firstSection = snd <$> splitVRatio 2 (splitVRatio 2 blank title) (splitVRatio 2 avatorInput nameInput)

      sencodSection = Bio <<$>> inputAreaWithPlaceHolder "Short bio about you"

      emailInput =
        Email <<$>> do
          Email email' <- sample $ current dEmail
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
        splitVRatio 2 (splitVRatio 2 emailInput pwInput) $ splitVRatio 2 updateButton logoutButton

  ((dAvatorInput, dNameInput), (dBioInput, ((dEmailInput, dPwInput), (eUpdate, eLogout)))) <- splitV3 firstSection sencodSection thirdSection

  pure $
    (,eLogout) $
      tag
        ( UpdateUser
            <$> current
              ( construct $
                  build @(Patch (UpdateOf 'User))
                    (pure . pure <$> dEmailInput)
                    (pure . pure <$> dPwInput)
                    (pure . pure <$> dNameInput)
                    (pure . pure <$> dBioInput)
                    (pure . pure <$> dAvatorInput)
              )
        )
        eUpdate
