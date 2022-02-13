{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Component.ArticleEditBox where

import Client (createArticleClient, getArticleClient, updateArticleClient)
import Control.Monad.Fix (MonadFix)
import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile (..))
import Data.Field.Body (Body (Body, unBody))
import Data.Field.Description (Description (Description, unDescription))
import Data.Field.Tag (splitToTags, tagsToText)
import Data.Field.Title (Title (Title, unTitle))
import Data.Generic.HKD (build, construct)
import Data.Generics.Product.Fields (getField)
import qualified Data.Semigroup as SG
import Data.Storage.Map (CreateOf (ArticleCreate), Patch, UpdateOf, toArticleId)
import Data.Text.Zipper (fromText)
import Data.Token.HasToken (TokenOf)
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (unOut))
import Data.Util.Validation (ValidationErr)
import Graphics.Vty (bold, green, withBackColor, withStyle)
import InVty.Component.InputBox (PlaceHolderMode (Edit, Replace), inputWithPlaceHolder)
import InVty.Util (ArticleIdOrContent, centerText, noBorderStyle, runRequestE, splitH3, splitV3)
import Reflex
  ( Adjustable,
    Dynamic,
    Event,
    MonadHold,
    PerformEvent,
    Performable,
    Reflex,
    current,
    fanEither,
    leftmost,
    never,
    now,
    switchDyn,
    (<@),
  )
import Reflex.Vty (HasDisplayRegion, HasFocus, HasFocusReader, HasImageWriter, HasInput, HasLayout, HasTheme, blank, boxStatic, button, def, doubleBoxStyle, localTheme, multilineTextInput, singleBoxStyle, text, textInput, _buttonConfig_focusStyle)
import Reflex.Workflow (Workflow (Workflow), workflow)
import Servant.Client (ClientEnv, ClientError)
import Validation (Validation (Failure, Success), maybeToSuccess)

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
    PerformEvent t m,
    MonadIO (Performable m)
  ) =>
  ClientEnv ->
  Maybe ArticleIdOrContent ->
  Dynamic t (TokenOf 'User) ->
  m (Event t ValidationErr, Event t ClientError, Event t (Out ArticleWithAuthorProfile))
articleEditBox clientEnv mAid dToken = do
  let inputBoxWithPlaceHolder = inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle
      inputAreaWithPlaceHolder = inputWithPlaceHolder multilineTextInput singleBoxStyle doubleBoxStyle

      mkBoxHelper title titleInput descriptionInput bodyInput tagsInput doneTxt =
        let title' = localTheme ((`withStyle` bold) <$>) $ centerText text title
            firstSection = snd <$> splitV3 title' titleInput descriptionInput

            secondSection = bodyInput

            doneButton =
              snd . snd
                <$> splitH3
                  blank
                  blank
                  ( button def {_buttonConfig_focusStyle = pure noBorderStyle} . localTheme ((`withBackColor` green) <$>) $
                      boxStatic noBorderStyle $ centerText text doneTxt
                  )

            thirdSection = splitV3 tagsInput doneButton blank
         in splitV3 firstSection secondSection thirdSection

  case mAid of
    Nothing -> do
      ((dMTitle, dMDescription), (dMBody, (dMTags, (ePublish, _)))) <-
        mkBoxHelper
          "New Article"
          (fmap Title <<$>> inputBoxWithPlaceHolder Replace "Article Title")
          (fmap Description <<$>> inputBoxWithPlaceHolder Replace "What's this article about?")
          (fmap Body <<$>> inputAreaWithPlaceHolder Replace "Wrtie your article (in markdown)")
          (fmap splitToTags <<$>> inputAreaWithPlaceHolder Replace "Enter tags")
          "Publish Article"
      let bVCreateArticle = current $ do
            vTitle <- maybeToSuccess ("empty title" :| []) <$> dMTitle
            vDescription <- maybeToSuccess ("empty description" :| []) <$> dMDescription
            vBody <- maybeToSuccess ("empty body" :| []) <$> dMBody
            vTags <- Success . fromMaybe [] <$> dMTags
            pure $ ArticleCreate <$> vTitle <*> vDescription <*> vBody <*> vTags
          (fanEither @_ @ValidationErr -> (eVErr, eRequest)) =
            ( bVCreateArticle >>= \case
                Failure errs -> pure $ Left errs
                ok -> Right <$> (createArticleClient <$> current dToken ?? In ok)
            )
              <@ ePublish
      (eErr, eRes) <- runRequestE clientEnv eRequest
      pure (eVErr, eErr, eRes)
    Just aidOrAcontent -> do
      (eErr1, eRes') <- case aidOrAcontent of
        Left aid ->
          second (unOut <$>) <$> do
            eNow <- now
            runRequestE clientEnv $ getArticleClient <$> current dToken ?? Success aid <@ eNow
        Right content -> (never,) . (content <$) <$> now
      let wf1 = Workflow $ do
            blank
            pure (never, wf2 <$> eRes')
          wf2 a@(getField @"article" @_ @ArticleWithAuthorProfile -> a') =
            Workflow $
              do
                -- FIXME TEMP tags ui and logic
                ((dMTitle, dMDescription), (dMBody, (dMTags, (eUpdate, _)))) <-
                  mkBoxHelper
                    "Edit Article"
                    (fmap Title <<$>> inputBoxWithPlaceHolder Edit (fromText . unTitle $ getField @"title" a'))
                    (fmap Description <<$>> inputBoxWithPlaceHolder Edit (fromText . unDescription $ getField @"description" a'))
                    (fmap Body <<$>> inputBoxWithPlaceHolder Edit (fromText . unBody $ getField @"body" a'))
                    (fmap splitToTags <<$>> inputAreaWithPlaceHolder Edit (fromText . tagsToText $ getField @"tagList" a))
                    "Save Changes"
                -- FIXME TEMP validation ??
                let bArticleUpdate =
                      current . construct $
                        build
                          @(Patch (UpdateOf 'Article))
                          (SG.Last <<$>> dMTitle)
                          (SG.Last <<$>> dMDescription)
                          (SG.Last <<$>> dMBody)
                    eRequest =
                      ( updateArticleClient
                          <$> current dToken
                            ?? Success (toArticleId a')
                            <*> (In . Success <$> bArticleUpdate)
                      )
                        <@ eUpdate
                (eErr, eOk) <- runRequestE clientEnv eRequest
                pure (leftmost [Left <$> eErr, Right <$> eOk], never)

      (eErr2, eRes) <- fanEither . switchDyn <$> workflow wf1
      pure (never, leftmost [eErr1, eErr2], eRes)
