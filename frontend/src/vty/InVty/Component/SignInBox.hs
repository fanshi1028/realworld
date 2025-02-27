{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Component.SignInBox where

import Client (loginClient)
import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasAuth (LoginOf (UserLogin))
import Data.Domain.User (UserAuthWithToken)
import Data.Field.Email (Email (Email))
import Data.Field.Password (mkPassword)
import Data.Text (center)
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out)
import Data.Util.Validation (ValidationErr)
import Graphics.Vty (bold, green, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Util (Go (Go), Page (SignUpPage), noBorderStyle, padText, runRequestE)
import InVty.Util.Split (splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, PerformEvent (Performable), Reflex, current, fanEither, (<@))
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, linkStatic, localTheme, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Servant.API (Header, Headers)
import Servant.Client (ClientEnv, ClientError)
import Validation (Validation (Failure), maybeToSuccess)
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
      Event t ValidationErr,
      Event t (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out UserAuthWithToken))
    )
signInBox clientEnv = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      title = localTheme ((`withStyle` bold) <$>) $ padText center text "Sign in"
      needAnAcc = (Go SignUpPage <$) <$> localTheme ((`withForeColor` green) <$>) (linkStatic "Need an account?")
      emailInput = fmap Email <<$>> inputBoxWithPlaceHolder Replace "Email"
      pwInput = fmap mkPassword <<$>> inputBoxWithPlaceHolder Replace "Password"
      signInButton =
        snd . snd
          <$> ( splitH3 blank blank $
                  button def {_buttonConfig_focusStyle = pure noBorderStyle} $
                    localTheme ((`withBackColor` green) <$>) $
                      boxStatic noBorderStyle $ padText center text "Sign In"
              )
  (_, (eGo, (dMEmailInput, (dMPwInput, (eSignIn, _))))) <-
    splitVRatio 5 title $
      splitVRatio 10 needAnAcc . splitVRatio 6 emailInput . splitVRatio 5 pwInput $
        splitVRatio 4 signInButton blank
  let bVUserLogin = current $ do
        bEmailInput <- maybeToSuccess ("empty email" :| []) <$> dMEmailInput
        bPwInput <- maybeToSuccess ("empty password" :| []) <$> dMPwInput
        pure $ UserLogin <$> bEmailInput <*> bPwInput
      (fanEither -> (eVErrs, ePayload)) =
        ( \case
            Failure errs -> Left errs
            ok -> Right $ In ok
        )
          <$> bVUserLogin
          <@ eSignIn
  (eErr, eRes) <- runRequestE clientEnv $ loginClient <$> ePayload
  pure (eGo, eErr, eVErrs, eRes)
