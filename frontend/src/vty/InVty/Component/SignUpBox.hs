{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Util.Validation (ValidationErr)
import Graphics.Vty (bold, green, withBackColor, withForeColor, withStyle)
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Util (Go (Go), Page (SignIn), centerText, noBorderStyle, runRequestE, splitH3, splitVRatio)
import Reflex (Adjustable, Event, MonadHold, PerformEvent, Performable, Reflex, current, fanEither, (<@))
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, linkStatic, localTheme, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Servant.Client (ClientError)
import Servant.Client.Streaming (ClientEnv)
import Validation (Validation (Failure), maybeToSuccess)

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

      usernameInput = fmap Username <<$>> inputBoxWithPlaceHolder Replace "Your name"

      emailInput = fmap Email <<$>> inputBoxWithPlaceHolder Replace "Email"

      pwInput = fmap mkPassword <<$>> inputBoxWithPlaceHolder Replace "Password"

      signUpButton =
        snd . snd
          <$> splitH3
            blank
            blank
            ( button def {_buttonConfig_focusStyle = pure noBorderStyle} $
                localTheme ((`withBackColor` green) <$>) $
                  boxStatic noBorderStyle $ centerText text "Sign Up"
            )

  (_, (eGoSignIn, (dMNameInput, (dMEmailInput, ((dMPwInput, eSignUp), _))))) <-
    splitVRatio 5 title $
      splitVRatio
        10
        haveAnAcc
        $ splitVRatio 6 usernameInput $
          splitVRatio 5 emailInput $
            splitVRatio 2 (splitVRatio 2 pwInput signUpButton) blank

  -- TEMP FIXME need Validaton, so refactor out them in backend??
  let bVUserCreate = current $ do
        dName <- maybeToSuccess ("empty name" :| []) <$> dMNameInput
        dEmail <- maybeToSuccess ("empty email" :| []) <$> dMEmailInput
        dPw <- maybeToSuccess ("empty password" :| []) <$> dMPwInput
        pure $ UserCreate <$> dName <*> dEmail <*> dPw

      (fanEither @_ @ValidationErr -> (eVErrs, ePayload)) =
        ( \case
            Failure errs -> Left errs
            ok -> Right $ In ok
        )
          <$> bVUserCreate
          <@ eSignUp

  (eErr, eRes) <- runRequestE clientEnv $ registerClient <$> ePayload

  -- TEMP FIXME This validation should output validtion error event too, but ignore it for now, we will fix it later.
  pure (Go SignIn <$ eGoSignIn, eErr, eRes)
