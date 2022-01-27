{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Component.SignInBox where

import Client (loginClient)
import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (LoginOf (UserLogin))
import Data.Domain.User (UserAuthWithToken)
import Data.Field.Email (Email (Email))
import Data.Field.Password (mkPassword)
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out)
import Graphics.Vty (bold, green, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (inputWithPlaceHolder)
import Reflex (Adjustable, Event, MonadHold, PerformEvent (Performable), Reflex, current, (<@))
import InVty.Util (Go (Go), Page (SignUp), centerText, noBorderStyle, runRequestE, splitH3, splitVRatio)
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, linkStatic, localTheme, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Servant.API (Header, Headers)
import Servant.Client (ClientEnv, ClientError)
import Validation (Validation (Success))
import Web.Cookie (SetCookie)

-- | @since 0.4.0.0
signInBox ::
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
      Event t (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out UserAuthWithToken))
    )
signInBox clientEnv = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      title = localTheme ((`withStyle` bold) <$>) $ centerText text "Sign in"
      needAnAcc = localTheme ((`withForeColor` green) <$>) $ linkStatic "Need an account?"
      emailInput = Email <<$>> inputBoxWithPlaceHolder "Email"
      pwInput = mkPassword <<$>> inputBoxWithPlaceHolder "Password"
      signInButton =
        snd . snd
          <$> ( splitH3 blank blank $
                  button def {_buttonConfig_focusStyle = pure noBorderStyle} $
                    localTheme ((`withBackColor` green) <$>) $
                      boxStatic noBorderStyle $ centerText text "Sign In"
              )
  (_, (eGoSignUp, (dEmailInput, (dPwInput, (eSignIn, _))))) <-
    splitVRatio 5 title $
      splitVRatio 10 needAnAcc . splitVRatio 6 emailInput . splitVRatio 5 pwInput $
        splitVRatio 4 signInButton blank
  let ePayload =
        In . Success
          <$> current (UserLogin <$> dEmailInput <*> dPwInput)
          <@ eSignIn
  (eErr, eRes) <- runRequestE clientEnv $ loginClient <$> ePayload
  pure (Go SignUp <$ eGoSignUp, eErr, eRes)
