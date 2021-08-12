{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.ToOne.Pure (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.Singletons.Bool (SBoolI (sbool), fromSBool)
import GHC.TypeLits (Symbol)
import Relation.ToOne (E (GetRelated, IsRelated, Relate, Unrelate))

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (b :: Bool)
    (m :: Type -> Type)
    a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, SBoolI b) => Algebra (E r1 r r2 :+: sig) (C r1 r r2 b m) where
  alg _ (L Relate {}) ctx = pure $ () <$ ctx
  alg _ (L Unrelate {}) ctx = pure $ () <$ ctx
  alg _ (L IsRelated {}) ctx = pure $ fromSBool (sbool @b) <$ ctx
  -- FIXME
  alg _ (L GetRelated {}) ctx = pure $ bool Nothing (Just undefined) (fromSBool $ sbool @b) <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
