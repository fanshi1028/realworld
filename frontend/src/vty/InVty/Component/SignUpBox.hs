{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Component.SignUpBox where

import Client (registerClient)
import Control.Monad.Fix (MonadFix)
import Data.Domain.User (UserAuthWithToken)
import Data.Field.Email (Email (Email))
import Data.Field.Password (mkPassword)
import Data.Field.Username (Username (Username))
import Data.Storage.Map (CreateOf (UserCreate))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out)
import Graphics.Vty (bold, green, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (inputWithPlaceHolder)
import InVty.Util (Go (Go), Page (SignIn), centerText, noBorderStyle, runRequestE, splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, Reflex, current, (<@))
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, linkStatic, localTheme, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Servant.Client (ClientError)
import Servant.Client.Streaming (ClientEnv)
import Validation (Validation (Success))

-- | @since 0.4.0.0
signUpBox ::
  ( Reflex t,
    MonadFix m,
    HasInput t m,
    HasFocusReader t m,
    HasTheme t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    Adjustable t m,
    HasFocus t m,
    HasLayout t m,
    MonadHold t m,
    MonadIO (Performable m),
    PerformEvent t m
  ) =>
  ClientEnv ->
  m
    ( Event t Go,
      Event t ClientError,
      Event t (Out UserAuthWithToken)
    )
signUpBox clientEnv = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle

      title = localTheme ((`withStyle` bold) <$>) $ centerText text "Sign up"

      haveAnAcc = localTheme ((`withForeColor` green) <$>) $ linkStatic "Have an account?"

      usernameInput = Username <<$>> inputBoxWithPlaceHolder "Your name"

      emailInput = Email <<$>> inputBoxWithPlaceHolder "Email"

      pwInput = mkPassword <<$>> inputBoxWithPlaceHolder "Password"
      signUpButton =
        snd . snd
          <$> splitH3
            blank
            blank
            ( button def {_buttonConfig_focusStyle = pure noBorderStyle} $
                localTheme ((`withBackColor` green) <$>) $
                  boxStatic noBorderStyle $ centerText text "Sign Up"
            )
  (_, (eGoSignIn, (dNameInput, (dEmailInput, ((dPwInput, eSignUp), _))))) <-
    splitVRatio 5 title $
      splitVRatio
        10
        haveAnAcc
        $ splitVRatio 6 usernameInput $
          splitVRatio 5 emailInput $
            splitVRatio 2 (splitVRatio 2 pwInput signUpButton) blank

  let ePayload =
        In . Success
          <$> current (UserCreate <$> dNameInput <*> dEmailInput <*> dPwInput)
          <@ eSignUp
  (eErr, eRes) <- runRequestE clientEnv $ registerClient <$> ePayload
  pure (Go SignIn <$ eGoSignIn, eErr, eRes)
