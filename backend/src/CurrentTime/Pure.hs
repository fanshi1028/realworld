{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module CurrentTime.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import CurrentTime (E (GetCurrentTime))
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m) => Algebra (E :+: sig) (C m) where
  alg _ (L GetCurrentTime) ctx = pure (UTCTime (ModifiedJulianDay 0) 0 <$ ctx)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
