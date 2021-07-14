{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenID.UUID.V4 where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.TypeLits (Symbol)
import GenID (E (GenerateID))

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance
  ( MonadIO m,
    Coercible (r "id") UUID,
    Algebra sig m
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg _ (L (GenerateID _)) ctx = (<$ ctx) . coerce <$> liftIO nextRandom
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
