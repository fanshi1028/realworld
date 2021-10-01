{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : __UNSAFE__
-- Copyright   : (c) 2021 fanshi1028
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

{-# WARNING runIOinSTM, runSTMinIO "This is unsafe; I hope you are sure that your IO are safe to use in STM" #-}

-- | It runs each lifted IO effects,
-- embedding them unsafely into STM effects,
-- then combine all the STM effect into run single STM stack and run it.
-- If you sure the side effect of your IO are safe,
-- this runner is what you should bet on.
--
-- @since 0.2.0.0
newtype C' a = C'
  { runIOinSTM :: STM a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance Algebra (Lift STM :+: Lift IO) C' where
  alg hdl (L (LiftWith with)) = C' . with (runIOinSTM . hdl)
  alg hdl (R io) = C' . unsafeIOToSTM . alg (atomically . runIOinSTM . hdl) io
  {-# INLINE alg #-}

-- | @since 0.2.0.0
newtype C'' a = C''
  { runSTMinIO :: IO a
  }
  deriving (Functor, Applicative, Monad)

-- | This is __99% not__ what you want.
-- It runs each lifted STM effect separately, which turn consecutive STM effects into consecutive IO effects.
-- Then why bother with STM in the first place??
--
-- @since 0.1.0.0
instance Algebra (Lift STM :+: Lift IO) C'' where
  alg hdl (L (LiftWith with)) = C'' . atomically . with (unsafeIOToSTM . runSTMinIO . hdl)
  alg hdl (R io) = C'' . alg (runSTMinIO . hdl) io
  {-# INLINE alg #-}
