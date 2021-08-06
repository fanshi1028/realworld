{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Tag.InMem (run) where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Domain.Util.Field (Tag)
import qualified Storage.Set (E (GetAll))
import Tag (E (GetTags))

newtype C m a = C {run :: m a}
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Member (Storage.Set.E Tag) sig
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L GetTags) ctx = (<$ ctx) <$> send (Storage.Set.GetAll @Tag)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
