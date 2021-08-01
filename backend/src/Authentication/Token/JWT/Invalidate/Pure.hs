{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Token.JWT.Invalidate.Pure where

import Authentication.Token.JWT.Invalidate (E (Invalidate))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)

newtype C (r :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (E r :+: sig) (C r m) where
  alg _ (L (Invalidate _)) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
