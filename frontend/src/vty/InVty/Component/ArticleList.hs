{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module InVty.Component.ArticleList where

import Client (getArticlesClient, getFeedsClient)
import Control.Monad.Fix (MonadFix)
import Data.Domain (Domain (User))
import Data.Domain.Article (ArticleWithAuthorProfile (..))
import Data.Field.Description (Description (unDescription))
import Data.Field.Tag (Tag (Tag, unTag))
import Data.Field.Title (Title (unTitle))
import Data.Field.Username (Username (Username))
import Data.Generics.Product (getField)
import Data.Storage.Map (IdOf (UserId))
import Data.Text (justifyRight)
import qualified Data.Text as T (length)
import Data.Token.HasToken (TokenOf (UserToken))
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Component.Navbar (buttonCfg, tabCfg)
import InVty.Component.Tab (Tabable (toTabKey, toTabName), mkTab, mkTab')
import InVty.Util (Go (Go), Page (ArticleContentPage, ProfilePage), padText, runRequestD)
import Reflex
  ( Event,
    PerformEvent,
    Performable,
    PostBuild,
    TriggerEvent,
    current,
    debounce,
    holdDyn,
    leftmost,
    listHoldWithKey,
    simpleList,
    switchDyn,
    updated,
    (<@),
  )
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
    link,
    row,
    singleBoxStyle,
    text,
    textInput,
    tile,
  )
import Servant.Client (ClientEnv)
import Validation (Validation (Success))

-- | @since 0.4.0.0
data ArticleListTab = Feeds | Global | ByTag Tag

-- | @since 0.4.0.0
instance Tabable ArticleListTab where
  toTabKey Feeds = 1
  toTabKey Global = 2
  toTabKey (ByTag (Tag _)) = 3
  toTabName Feeds = "Your Feed"
  toTabName Global = "Global Feed"
  toTabName (ByTag (Tag t)) = "#" <> t

mkArticlePreview ::
  ( HasInput t m,
    HasImageWriter t m,
    HasDisplayRegion t m,
    HasTheme t m,
    HasFocusReader t m,
    HasLayout t m,
    HasFocus t m,
    MonadFix m,
    Adjustable t m,
    MonadHold t m,
    PostBuild t m
  ) =>
  Dynamic t ArticleWithAuthorProfile ->
  m (Event t Go, Event t ArticleListTab)
mkArticlePreview dArticle = do
  boxStatic singleBoxStyle . col $ do
    let a = getField @"article" <$> current dArticle
    eTilte <- tile (fixed 3) . button buttonCfg . text $ unTitle . getField @"title" <$> a
    eDescription <- tile flex . button buttonCfg . text $ unDescription . getField @"description" <$> a
    (deTagTab, eUserProfile) <- tile (fixed 3) . row $ do
      deTagTab <-
        tile flex . row $
          simpleList (getField @"tagList" <$> dArticle) $ \dt -> do
            grout (fixed 1) blank
            eTagTab <-
              tile (fixed $ T.length . unTag <$> dt) $
                (ByTag <$> current dt <@) <$> link (unTag <$> current dt)
            grout (fixed 1) blank
            pure eTagTab
      eUserProfile <-
        (ProfilePage . Just . Right . getField @"author" <$> current dArticle <@) <$> do
          tile flex . button buttonCfg . padText justifyRight text $
            a <&> \(getField @"author" -> UserId (Username user)) -> "by: " <> user
      pure (deTagTab, eUserProfile)
    pure
      ( Go
          <$> leftmost
            [ ArticleContentPage . Right <$> current dArticle <@ leftmost [eTilte, eDescription],
              eUserProfile
            ],
        switchDyn $ leftmost <$> deTagTab
      )

-- | @since 0.4.0.0
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
    MonadIO (Performable m),
    TriggerEvent t m
  ) =>
  ClientEnv ->
  Maybe (Dynamic t (TokenOf 'User)) ->
  Event t (Maybe Tag) ->
  m (Event t Go)
articleList clientenv mDToken eMFilterTag = col $ do
  rec dSelectedTab <- tile (fixed 3) . row $ do
        rec let eMFilterByTag = leftmost [ByTag <<$>> eMFilterTag, eMFilterByTag', eMFilterByTag'']
                iniitSelectTab = maybe Global (const Feeds) mDToken
            dTabs <- listHoldWithKey
              (fromList $ (toTabKey &&& id) <$> maybe [Global] (const [Feeds, Global]) mDToken)
              ( one . first toTabKey
                  . maybe
                    (ByTag (Tag "no tag"), Nothing)
                    (id &&& Just)
                  <$> eMFilterByTag
              )
              $ \_key v -> pure v

            dSelectedTab' <-
              tile flex $
                mkTab' tabCfg (fixed 15) iniitSelectTab dTabs $
                  fromMaybe iniitSelectTab <$> eMFilterByTag

            eMFilterByTag' <-
              tile (fixed 25) $
                fmap (ByTag . Tag)
                  <<$>> inputWithPlaceHolder textInput singleBoxStyle doubleBoxStyle Replace "filter by tag"
                    >>= debounce 0.5 . updated

        dSelectedTab'
          <$ grout (fixed 1) blank

      (eErr, eRes) <- do
        let dToken = fromMaybe (pure $ UserToken "") mDToken
        runRequestD clientenv $
          dSelectedTab >>= \case
            Feeds -> getFeedsClient <$> dToken ?? Nothing ?? Nothing
            Global -> getArticlesClient <$> dToken ?? Nothing ?? Nothing ?? Nothing ?? Nothing ?? Nothing
            -- FIXME tag validation??
            ByTag t -> getArticlesClient <$> dToken ?? Just (Success t) ?? Nothing ?? Nothing ?? Nothing ?? Nothing

      dArticleList <- holdDyn [] $ leftmost [[] <$ eErr, unOut <$> eRes]

      ds <- col (simpleList dArticleList $ \dArticle -> tile (fixed 12) $ mkArticlePreview dArticle)

      let eMFilterByTag'' = switchDyn $ leftmost <$> (fmap Just . snd <<$>> ds)

  grout flex blank
  pure . switchDyn $ leftmost <$> (fst <<$>> ds)

-- | @since 0.4.0.0
data ProfileArticleListTab = MyArticles | FavouritedArticles

-- | @since 0.4.0.0
instance Tabable ProfileArticleListTab where
  toTabKey = \case
    MyArticles -> 1
    FavouritedArticles -> 2
  toTabName = \case
    MyArticles -> "My Articles"
    FavouritedArticles -> "Favourited Articles"

profileArticleList ::
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
    MonadIO (Performable m),
    TriggerEvent t m
  ) =>
  ClientEnv ->
  Dynamic t Username ->
  m (Event t Go, Event t ArticleListTab)
profileArticleList clientenv dUser = do
  dSelectedTab <-
    tile (fixed 3) . row $
      ( tile flex $
          mkTab tabCfg (fixed 15) MyArticles . pure $
            fromList ((toTabKey &&& id) <$> [MyArticles, FavouritedArticles])
      )
        <* grout (fixed 1) blank

  (eErr, eRes) <- do
    let token = UserToken ""
        dMVUser = Just . Success <$> dUser
    runRequestD clientenv $
      dSelectedTab >>= \case
        -- FIXME TEMP
        MyArticles -> getArticlesClient token Nothing <$> dMVUser ?? Nothing ?? Nothing ?? Nothing
        FavouritedArticles -> getArticlesClient token Nothing Nothing <$> dMVUser ?? Nothing ?? Nothing

  dArticleList <- holdDyn [] $ leftmost [[] <$ eErr, unOut <$> eRes]

  ds <- col (simpleList dArticleList $ \dArticle -> tile (fixed 12) $ mkArticlePreview dArticle)

  grout flex blank

  pure
    ( switchDyn $ leftmost <$> (fst <<$>> ds),
      switchDyn $ leftmost <$> (snd <<$>> ds)
    )
