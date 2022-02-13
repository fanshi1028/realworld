{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @since 0.4.0.0
module InVty.Util where

import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.User (UserAuthWithToken, UserProfile)
import Data.Storage.Map (HasStorage (..))
import InVty.Component.Tab (Tabable (toTabKey, toTabName))
import Reflex (Behavior, Event, PerformEvent, Performable, Reflex, current, fanEither, performEvent)
import Reflex.Vty (BoxStyle (BoxStyle), HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, displayWidth, splitH, splitV)
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
splitHRatio :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => Int -> m a -> m b -> m (a, b)
splitHRatio n = splitH (pure (`div` n)) (pure (True, True))

splitH2 :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => m a -> m b -> m (a, b)
splitH2 = splitHRatio 2

-- | @since 0.4.0.0
splitH3 :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => m a -> m b -> m c -> m (a, (b, c))
splitH3 l m r = splitHRatio 3 l $ splitHRatio 2 m r

-- | @since 0.4.0.0
splitVRatio :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => Int -> m a -> m b -> m (a, b)
splitVRatio n = splitV (pure (`div` n)) (pure (True, True))

-- | @since 0.4.0.0
splitV3 :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => m a -> m b -> m c -> m (a, (b, c))
splitV3 l m r = splitVRatio 3 l $ splitVRatio 2 m r

-- | @since 0.4.0.0
noBorderStyle :: BoxStyle
noBorderStyle = BoxStyle ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

-- | @since 0.4.0.0
-- >>> padding 10 "fjowefew"
padding :: Int -> String -> String
padding width str = let pad = replicate ((width - length str) `div` 2) ' ' in pad <> str <> pad

-- | @since 0.4.0.0
centerText :: (Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => (Behavior t Text -> m ()) -> String -> m ()
centerText txtMaker txt = do
  w <- displayWidth
  txtMaker $ fromString <$> (padding <$> current w ?? txt)

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
