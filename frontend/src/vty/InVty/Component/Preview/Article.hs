{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.4.0.0
module InVty.Component.Preview.Article where

import Control.Monad.Fix (MonadFix)
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Field.Description (unDescription)
import Data.Field.Tag (Tag, unTag)
import Data.Field.Title (unTitle)
import Data.Field.Username (Username (Username))
import Data.Generics.Product (getField)
import Data.Storage.Map (IdOf (UserId))
import Data.Text (justifyRight)
import qualified Data.Text as T
import InVty.Util (Go (Go), Page (ArticleContentPage, ProfilePage), noBorderStyle, padText)
import Reflex (Adjustable, Dynamic, Event, MonadHold, PostBuild, current, leftmost, simpleList, switchDyn, (<@))
import Reflex.Vty
  ( ButtonConfig (ButtonConfig),
    HasDisplayRegion,
    HasFocus,
    HasFocusReader,
    HasImageWriter,
    HasInput,
    HasLayout,
    HasTheme,
    blank,
    boxStatic,
    button,
    col,
    fixed,
    flex,
    grout,
    link,
    row,
    singleBoxStyle,
    text,
    tile,
  )

-- | @since 0.4.0.0
articlePreview ::
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
  m (Event t Go, Event t Tag)
articlePreview dArticle = do
  boxStatic singleBoxStyle . col $ do
    let a = getField @"article" <$> current dArticle
        buttonCfg = ButtonConfig (pure noBorderStyle) (pure noBorderStyle)
    eTilte <- tile (fixed 3) . button buttonCfg . text $ unTitle . getField @"title" <$> a
    eDescription <- tile flex . button buttonCfg . text $ unDescription . getField @"description" <$> a
    (deTag, eUserProfile) <- tile (fixed 3) . row $ do
      deTag <-
        tile flex . row $
          simpleList (getField @"tagList" <$> dArticle) $ \dt -> do
            grout (fixed 1) blank
            eTagTab <-
              tile (fixed $ T.length . unTag <$> dt) $
                (current dt <@) <$> link (unTag <$> current dt)
            grout (fixed 1) blank
            pure eTagTab
      eUserProfile <-
        (ProfilePage . Just . Right . getField @"author" <$> current dArticle <@) <$> do
          tile flex . button buttonCfg . padText justifyRight text $
            a <&> \(getField @"author" -> UserId (Username user)) -> "by: " <> user
      pure (deTag, eUserProfile)
    pure
      ( Go
          <$> leftmost
            [ ArticleContentPage . Right <$> current dArticle <@ leftmost [eTilte, eDescription],
              eUserProfile
            ],
        switchDyn $ leftmost <$> deTag
      )
