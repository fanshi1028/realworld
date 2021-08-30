{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : __UNSAFE__
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- __HACK__: mix IO with STM, you are responsible for making sure those IO action are safe to run.
-- There are two carriers provided, neither are really safe, __use at your own risk__!
-- The runners should be the last of all your effect runner, which natural transform into a monad with 'MonadIO'.
--
-- __NOTE__: I think this module is __wrong__,
-- it relies on us,
-- the careless mortal too much,
-- to make sure it doing the right thing,
-- which is very non-haskell.
--
-- Let's fix this in the future, for now, be __really really careful__ to use this.
--
-- @since 0.1.0.0
module STMWithUnsafeIO (runIOinSTM, runSTMinIO) where

import Control.Algebra (Algebra (alg))
import Control.Effect.Lift (Lift (LiftWith))
import Control.Effect.Sum (type (:+:) (L, R))
import GHC.Conc (unsafeIOToSTM)

-- | @since 0.1.0.0
newtype C' a = C'
  { run' :: STM a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance Algebra (Lift STM :+: Lift IO) C' where
  alg hdl (L (LiftWith with)) = C' . with (run' . hdl)
  alg hdl (R io) = C' . unsafeIOToSTM . alg (atomically . run' . hdl) io
  {-# INLINE alg #-}

-- | It runs each lifted IO effects,
-- embedding them unsafely into STM effects,
-- then combine all the STM effect into run single STM stack and run it.
-- If you sure the side effect your IO are safe,
-- this runner is what you should bet on.
--
-- @since 0.1.0.0
runIOinSTM :: MonadIO m => C' a -> m a
runIOinSTM = run' >>> atomically

-- | @since 0.1.0.0
newtype C'' a = C''
  { run'' :: IO a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance Algebra (Lift STM :+: Lift IO) C'' where
  alg hdl (L (LiftWith with)) = C'' . atomically . with (unsafeIOToSTM . run'' . hdl)
  alg hdl (R io) = C'' . alg (run'' . hdl) io
  {-# INLINE alg #-}

-- | This is __99% not__ what you want.
-- It runs each lifted STM effect separately, which turn consecutive STM effects into consecutive IO effects.
-- Then why bother with STM in the first place??
--
-- @since 0.1.0.0
runSTMinIO :: MonadIO m => C'' a -> m a
runSTMinIO = run'' >>> liftIO
