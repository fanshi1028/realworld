{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenID.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Domain.Util.Representation (Transform (transform))
import GHC.TypeLits (Symbol)
import GenID (E (GenerateID))

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance
  ( Algebra sig m,
    Transform r "create" "id"
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg _ (L (GenerateID create)) ctx = pure $ transform create <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
