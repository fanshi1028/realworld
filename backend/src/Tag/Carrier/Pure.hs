{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Tag.Carrier.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Domain.Util.Field as F (Tag (Tag))
import Tag.Effect (Tag (GetTags))

newtype TagPure (f :: Type -> Type) m a = TagPure {runTagPure :: m a} deriving (Functor, Applicative, Monad)

instance Algebra sig m => Algebra (Tag [] :+: sig) (TagPure [] m) where
  alg _ (L GetTags) ctx = pure $ [F.Tag "This is a tag"] <$ ctx
  alg hdl (R other) ctx = TagPure $ alg (runTagPure . hdl) other ctx
