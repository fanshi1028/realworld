{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Current.Reader where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Current (E (GetCurrent))

newtype C e m a = C
  { run :: ReaderT e m a
  }
  deriving (Functor, Applicative, Monad, MonadReader e)

instance (Algebra sig m) => Algebra (E e :+: sig) (C e m) where
  alg _ (L GetCurrent) ctx = (<$ ctx) <$> ask
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
