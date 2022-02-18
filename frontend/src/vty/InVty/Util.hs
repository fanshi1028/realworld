{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Util where

import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.User (UserAuthWithToken, UserProfile)
import Data.Storage.Map (HasStorage (..))
import InVty.Component.Tab (Tabable (toTabKey, toTabName))
import Reflex
  ( Behavior,
    Dynamic,
    Event,
    PerformEvent,
    Performable,
    PostBuild,
    Reflex,
    current,
    fanEither,
    getPostBuild,
    leftmost,
    performEvent,
    updated,
    (<@),
  )
import Reflex.Vty
  ( BoxStyle (BoxStyle),
    HasDisplayRegion,
    HasImageWriter,
    HasTheme,
    displayWidth,
  )
import Servant.Client (ClientEnv, ClientError)
import Servant.Client.Streaming (ClientM, withClientM)

-- | @since 0.4.0.0
newtype LoggedIn = LoggedIn UserAuthWithToken

-- | @since 0.4.0.0
data LoggedOut = LoggedOut

-- | @since 0.4.0.0
type ArticleIdOrContent = Either (IdOf 'Article) ArticleWithAuthorProfile

-- | @since 0.4.0.0
type UserIdOrProfile = Either (IdOf 'User) UserProfile

-- | @since 0.4.0.0
data Page
  = -- | @since 0.4.0.0
    HomePage
  | -- | @since 0.4.0.0
    EditorPage (Maybe ArticleIdOrContent)
  | -- | @since 0.4.0.0
    SettingsPage
  | -- | @since 0.4.0.0
    ArticleContentPage ArticleIdOrContent
  | -- | @since 0.4.0.0
    ProfilePage (Maybe UserIdOrProfile)
  | -- | @since 0.4.0.0
    SignInPage
  | -- | @since 0.4.0.0
    SignUpPage
  deriving (Show, Eq)

-- | @since 0.4.0.0
instance Tabable Page where
  toTabKey HomePage = 1
  -- logged out
  toTabKey SignInPage = 2
  toTabKey SignUpPage = 3
  -- logged in
  toTabKey (EditorPage Nothing) = 2
  toTabKey SettingsPage = 3
  toTabKey (ProfilePage Nothing) = 4
  -- not tabable
  toTabKey a@(EditorPage (Just _)) = error $ "not tabable, to tab key failed: " <> show a
  toTabKey a@(ArticleContentPage _) = error $ "not tabable, to tab key failed: " <> show a
  toTabKey a@(ProfilePage (Just _)) = error $ "not tabable, to tab key failed: " <> show a
  toTabName HomePage = "Home"
  -- logged out
  toTabName SignInPage = "Sign in"
  toTabName SignUpPage = "Sign up"
  -- logged in
  toTabName (EditorPage Nothing) = "New article"
  toTabName SettingsPage = "Settings"
  toTabName (ProfilePage Nothing) = "Who am I"
  -- not tabable
  toTabName a@(EditorPage (Just _)) = error $ "not tabable, to tab name failed: " <> show a
  toTabName a@(ArticleContentPage _) = error $ "not tabable, to tab name failed: " <> show a
  toTabName a@(ProfilePage (Just _)) = error $ "not tabable, to tab name failed: " <> show a

-- | @since 0.4.0.0
newtype Go = Go Page deriving (Eq)

-- | @since 0.4.0.0
noBorderStyle :: BoxStyle
noBorderStyle = BoxStyle ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

-- | @since 0.4.0.0
padText ::
  (Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) =>
  (Int -> Char -> Text -> Text) ->
  (Behavior t Text -> m ()) ->
  Behavior t Text ->
  m ()
padText padder txtMaker bTxt = do
  dW <- displayWidth
  txtMaker $ padder <$> current dW ?? ' ' <*> bTxt

-- | @since 0.4.0.0
runRequestE ::
  ( Reflex t,
    MonadIO (Performable m),
    PerformEvent t m
  ) =>
  ClientEnv ->
  Event t (ClientM a) ->
  m (Event t ClientError, Event t a)
runRequestE clientEnv eRequest =
  fanEither
    <$> performEvent
      (liftIO <$> (flip withClientM clientEnv <$> eRequest ?? pure))

-- | @since 0.4.0.0
runRequestD ::
  ( PostBuild t m,
    MonadIO (Performable m),
    PerformEvent t m
  ) =>
  ClientEnv ->
  Dynamic t (ClientM a) ->
  m (Event t ClientError, Event t a)
runRequestD clientenv dRequest =
  getPostBuild >>= \eNow ->
    runRequestE clientenv $
      leftmost
        [ updated dRequest,
          current dRequest <@ eNow
        ]
