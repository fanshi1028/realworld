{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenID.UUID.V1 where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.UUID.V1 (nextUUID)
import GHC.TypeLits (Symbol)
import GenID (E (GenerateID))
import Data.UUID (UUID)

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance
  ( MonadIO m,
    Member (Throw Text) sig,
    Coercible (r "id") UUID,
    Algebra sig m
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg _ (L (GenerateID _)) ctx = (<$ ctx) <$> (liftIO nextUUID >>= maybe (throwError @Text "requested UUIDs too quickly") pure . coerce)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
