{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Current.Pure (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Current (E (GetCurrent))
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Domain.User (UserR)
import Domain.Util.Field (Time)

newtype C e m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (E Time :+: sig) (C Time m) where
  alg _ (L GetCurrent) ctx = pure (UTCTime (ModifiedJulianDay 0) 0 <$ ctx)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E (UserR s) :+: sig) (C (UserR s) m) where
  alg _ (L GetCurrent) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
