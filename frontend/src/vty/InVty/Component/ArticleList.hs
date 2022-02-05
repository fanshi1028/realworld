{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- |
module InVty.Component.ArticleList where

import Client (getArticlesClient, getFeedsClient)
import Control.Monad.Fix (MonadFix)
import Data.Domain (Domain (User))
import Data.Domain.Article (ArticleWithAuthorProfile (..))
import Data.Field.Description (Description (unDescription))
import Data.Field.Tag (Tag (Tag, unTag), tagsToText)
import Data.Field.Title (Title (unTitle))
import Data.Generics.Product (getField)
import Data.Token.HasToken (TokenOf (UserToken))
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Component.Navbar (buttonCfg, tabCfg)
import InVty.Component.Tab (mkTab)
import InVty.Util (Go (Go), Page (ArticleContentPage), runRequestE)
import Reflex (Event, PerformEvent, Performable, PostBuild (getPostBuild), Reflex (Behavior), current, holdDyn, leftmost, listHoldWithKey, simpleList, switchDyn, updated, (<@))
import Reflex.Vty
  ( Adjustable,
    HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    MonadHold,
    Reflex (Dynamic),
    blank,
    boxStatic,
    button,
    col,
    doubleBoxStyle,
    fixed,
    flex,
    grout,
    row,
    singleBoxStyle,
    text,
    textInput,
    tile,
  )
import Servant.Client (ClientEnv)
import Validation (Validation (Success))

mkArticlePreview ::
  ( HasInput t m,
    HasImageWriter t m,
    HasDisplayRegion t m,
    HasTheme t m,
    HasFocusReader t m,
    HasLayout t m,
    HasFocus t m,
    MonadFix m
  ) =>
  Behavior t ArticleWithAuthorProfile ->
  m (Event t Go)
mkArticlePreview bArticle =
  (Go . ArticleContentPage . Right <$> bArticle <@) <$> do
    button buttonCfg . boxStatic singleBoxStyle . col $ do
      -- NOTE: TEMP FIXME
      -- { article :: ContentOf 'Article,
      --   tagList :: [Tag],
      --   favorited :: Bool,
      --   favoritesCount :: Natural,
      --   author :: UserProfile
      -- }
      -- { title :: Title, -- "How to train your dragon",
      --   description :: Description, -- "Ever wonder how?",
      --   body :: Body, -- "It takes a Jacobian",
      --   createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
      --   updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
      --   author :: IdOf 'User
      -- }
      let a = getField @"article" <$> bArticle
      tile flex $ text $ unTitle . getField @"title" <$> a
      tile flex $ text $ unDescription . getField @"description" <$> a
      tile flex $ text $ show . getField @"author" <$> a
      tile flex $ text $ tagsToText . getField @"tagList" <$> bArticle

data ArticleListTab = Feeds | Global | ByTag Tag

articleList ::
  ( HasDisplayRegion t m,
    HasFocusReader t m,
    HasTheme t m,
    HasImageWriter t m,
    HasInput t m,
    Adjustable t m,
    PostBuild t m,
    HasFocus t m,
    HasLayout t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    MonadIO (Performable m)
  ) =>
  ClientEnv ->
  Maybe (Dynamic t (TokenOf 'User)) ->
  Dynamic t (Maybe Tag) ->
  m (Event t Go)
articleList clientenv mDToken dMFilterTag =
  (switchDyn <$>) . (leftmost <<$>>) . col $ do
    dSelectedTab <- tile (fixed 3) . row $ do
      let yourFeedTab = [(1, ("Your Feed", Feeds))]
          globalFeedTab = [(2, ("Global Feed", Global))]
      rec dTabs <- listHoldWithKey
            (fromList $ maybe globalFeedTab (const $ yourFeedTab <> globalFeedTab) mDToken)
            ( fromList
                . maybe
                  [(3, Nothing)]
                  (\t -> [(3, Just ("#" <> unTag t, ByTag t))])
                <$> leftmost [updated dMFilterTag, updated dMFilterTag']
            )
            $ \_key v -> pure v

          dSelectedTab' <-
            tile flex $
              mkTab tabCfg (fixed 15) 2 dTabs
                >>= holdDyn
                  (maybe Global (const Feeds) mDToken)

          dMFilterTag' <-
            tile (fixed 25) $
              fmap Tag
                <<$>> inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle Replace "filter by tag"

      dSelectedTab' <$ grout (fixed 1) blank

    let dToken = fromMaybe (pure $ UserToken "") mDToken
        dRequest =
          dSelectedTab >>= \case
            Feeds -> getFeedsClient <$> dToken ?? Nothing ?? Nothing
            Global -> getArticlesClient <$> dToken ?? Nothing ?? Nothing ?? Nothing ?? Nothing ?? Nothing
            -- FIXME tag validation??
            ByTag t -> getArticlesClient <$> dToken ?? Just (Success t) ?? Nothing ?? Nothing ?? Nothing ?? Nothing

    (eErr, eRes) <-
      getPostBuild >>= \eNow ->
        runRequestE clientenv $
          leftmost
            [ current dRequest <@ eNow,
              updated dRequest
            ]

    dArticleList <- holdDyn [] $ leftmost [[] <$ eErr, unOut <$> eRes]

    col (simpleList dArticleList $ \dArticle -> tile (fixed 12) . mkArticlePreview $ current dArticle)
      <* grout flex blank

-- sortableList
