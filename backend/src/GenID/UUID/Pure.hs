{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenID.UUID.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.UUID (UUID, nil)
import GHC.TypeLits (Symbol)
import GenID (E (GenerateID))

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Coercible (r "id") UUID
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg _ (L (GenerateID _)) ctx = pure $ coerce nil <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
