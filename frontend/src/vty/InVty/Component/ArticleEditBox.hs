{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Component.ArticleEditBox where

import Client (createArticleClient)
import Control.Monad.Fix (MonadFix)
import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Field.Body (Body (Body))
import Data.Field.Description (Description (Description))
import Data.Field.Title (Title (Title))
import Data.Storage.Map (CreateOf (ArticleCreate), HasStorage (IdOf), IdOf (ArticleId))
import Data.Token.HasToken (TokenOf)
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out)
import Graphics.Vty (bold, green, withBackColor, withStyle)
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Util (centerText, noBorderStyle, runRequestE, splitH3, splitV3, splitVRatio)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PerformEvent, Performable, Reflex, current, (<@))
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, localTheme, multilineTextInput, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Servant.Client (ClientEnv, ClientError)
import Validation (Validation (Success))

-- | @since 0.4.0.0
articleEditBox ::
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
  Maybe (IdOf 'Article) ->
  Dynamic t (TokenOf 'User) ->
  m (Event t ClientError, Event t (Out ArticleWithAuthorProfile))
articleEditBox clientEnv mAid dToken = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      inputAreaWithPlaceHolder = inputWithPlaceHolder multilineTextInput singleBoxStyle doubleBoxStyle

      title =
        localTheme ((`withStyle` bold) <$>) . centerText text $
          if isJust mAid then "Edit Article" else "New Article"
      titleInput = Title <<$>> inputBoxWithPlaceHolder Replace "Article Title"
      descriptionInput = Description <<$>> inputBoxWithPlaceHolder Replace "What's this article about?"
      firstSection = snd <$> splitV3 title titleInput descriptionInput

      secondSection = Body <<$>> inputAreaWithPlaceHolder Replace "Wrtie your article (in markdown)"

      tagsInput = inputAreaWithPlaceHolder Replace "Enter tags"

      publishButton =
        snd . snd
          <$> splitH3
            blank
            blank
            ( button def {_buttonConfig_focusStyle = pure noBorderStyle} . localTheme ((`withBackColor` green) <$>) $
                boxStatic noBorderStyle $ centerText text "Publish Article"
            )

      thirdSection = splitV3 tagsInput publishButton blank

  ((dTitle, dDescription), (dBody, (dTags, (ePublish, _)))) <- splitV3 firstSection secondSection thirdSection
  let dPayload = In . Success <$> (ArticleCreate <$> dTitle <*> dDescription <*> dBody ?? []) -- FIXME TEMP Tags
      eRequest = current (createArticleClient <$> dToken <*> dPayload) <@ ePublish
  runRequestE clientEnv eRequest
