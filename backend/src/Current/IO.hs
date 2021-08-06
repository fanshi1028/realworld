{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Current.IO where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Current (E (GetCurrent))
import Data.Time (getCurrentTime)
import Domain.Util.Field (Time)

newtype C e m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Member (Lift IO) sig) => Algebra (E Time :+: sig) (C Time m) where
  alg _ (L GetCurrent) ctx = (<$ ctx) <$> sendIO getCurrentTime
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
