-- | @since 0.4.0.0
module InVty.Util where

import Data.Field.Slug (Slug)
import Data.Field.Username (Username)
import Reflex (Behavior, current)
import Reflex.Vty (BoxStyle (BoxStyle), HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, HasTheme, displayWidth, splitH, splitV)

-- | @since 0.4.0.0
data LoginEvent = LoginEvent

-- | @since 0.4.0.0
data LogoutEvent = LogoutEvent
-- | @since 0.4.0.0
data Page = Home | NewArticle | Settings | Article Slug | Profile Username | SignIn | SignUp deriving (Eq)

-- | @since 0.4.0.0
newtype Go = Go Page deriving (Eq)

-- | @since 0.4.0.0
splitHRatio :: (HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => Int -> m a -> m b -> m (a, b)
splitHRatio n = splitH (pure (`div` n)) (pure (True, True))

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
