{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

-- |
module InVty.Component.List.Article where

import Client (getArticlesClient, getFeedsClient)
import Control.Monad.Fix (MonadFix)
import Data.Authentication.HasToken (TokenOf (UserToken))
import Data.Domain (Domain (User))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Username (Username)
import Data.Util.JSON.To (Out (unOut))
import InVty.Component.InputBox (PlaceHolderMode (Replace), inputWithPlaceHolder)
import InVty.Component.Navbar (tabCfg)
import InVty.Component.Preview.Article (articlePreview)
import InVty.Component.Tab (Tabable (toTabKey, toTabName), mkTab, mkTab')
import InVty.Util (Go, runRequestD)
import Reflex
  ( Event,
    PerformEvent,
    Performable,
    PostBuild,
    TriggerEvent,
    debounce,
    holdDyn,
    leftmost,
    listHoldWithKey,
    simpleList,
    switchDyn,
    updated,
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
    col,
    doubleBoxStyle,
    fixed,
    flex,
    grout,
    row,
    singleBoxStyle,
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

      ds <- col (simpleList dArticleList $ \dArticle -> tile (fixed 12) $ articlePreview dArticle)

      let eMFilterByTag'' = switchDyn $ leftmost <$> (fmap (Just . ByTag) . snd <<$>> ds)

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

  ds <- col (simpleList dArticleList $ \dArticle -> tile (fixed 12) $ articlePreview dArticle)

  grout flex blank

  pure
    ( switchDyn $ leftmost <$> (fst <<$>> ds),
      switchDyn $ leftmost <$> (fmap ByTag . snd <<$>> ds)
    )
