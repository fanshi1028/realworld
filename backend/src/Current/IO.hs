{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier in IO
--
-- @since 0.1.0.0
module Current.IO where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Current (E (GetCurrent))
import Data.Time (getCurrentTime)
import Domain.Util.Field (Time)

-- | @since 0.1.0.0
newtype C e m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Lift IO) sig) => Algebra (E Time :+: sig) (C Time m) where
  alg _ (L GetCurrent) ctx = (<$ ctx) <$> sendIO getCurrentTime
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
