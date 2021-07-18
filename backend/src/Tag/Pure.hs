{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Tag.Pure (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Domain.Util.Field (Tag (Tag))
import Tag (E (GetTags))

newtype C (f :: Type -> Type) m a = C {run :: m a} deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (E [] :+: sig) (C [] m) where
  alg _ (L GetTags) ctx = pure $ [Tag "This is a tag"] <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
