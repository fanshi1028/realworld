{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenID.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Domain.Util.Representation (Transform (transform))
import GHC.TypeLits (Symbol)
import GenID (E (GenerateID))

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Transform r "create" "id" (C r m)
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg _ (L (GenerateID create)) ctx = (<$ ctx) <$> transform create
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
