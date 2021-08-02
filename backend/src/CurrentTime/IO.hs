{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module CurrentTime.IO where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import CurrentTime (E (GetCurrentTime))
import Data.Time (getCurrentTime)

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Member (Lift IO) sig) => Algebra (E :+: sig) (C m) where
  alg _ (L GetCurrentTime) ctx = (<$ ctx) <$> sendIO getCurrentTime
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
