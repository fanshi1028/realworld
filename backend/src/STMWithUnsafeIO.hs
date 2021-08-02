{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | HACK NOTE mix IO with STM, you are responsible for making sure those IO action are safe for STM
module STMWithUnsafeIO where

import Control.Algebra (Algebra (alg))
import Control.Effect.Lift (Lift (LiftWith))
import Control.Effect.Sum (type (:+:) (L, R))
import GHC.Conc (unsafeIOToSTM)

newtype C a = C
  { run :: STM a
  -- { run :: IO a
  }
  deriving (Functor, Applicative, Monad)

-- HACK FIXME
instance Algebra (Lift STM :+: Lift IO) C where
  alg hdl (L (LiftWith with)) = C . with (run . hdl)
  alg hdl (R io) = C . unsafeIOToSTM . alg (atomically . run . hdl) io
  -- alg hdl (L (LiftWith with)) = C . atomically . with (unsafeIOToSTM . run . hdl)
  -- alg hdl (R io) = C . alg (run . hdl) io
