{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module CurrentTime.IO where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import CurrentTime (E (GetCurrentTime))
import Data.Time (getCurrentTime)

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (E :+: sig) (C m) where
  alg _ (L GetCurrentTime) ctx = (<$ ctx) <$> liftIO getCurrentTime
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
