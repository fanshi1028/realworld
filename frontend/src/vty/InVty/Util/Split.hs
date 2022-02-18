-- | @since 0.4.0.0
module InVty.Util.Split where

import Reflex.Vty (HasDisplayRegion, HasFocusReader, HasImageWriter, HasInput, splitH, splitV)

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
