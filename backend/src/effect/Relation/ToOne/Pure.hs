{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier in pure
--
-- @since 0.1.0.0
module Relation.ToOne.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.Singletons.Bool (SBoolI (sbool), fromSBool)
import GHC.TypeLits (Symbol)
import Relation.ToOne (E (GetRelated, Relate, Unrelate))

-- | @since 0.1.0.0
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

-- | @since 0.2.0.0
instance (Algebra sig m, SBoolI b) => Algebra (E r1 r r2 :+: sig) (C r1 r r2 b m) where
  alg _ (L Relate {}) ctx = pure $ () <$ ctx
  alg _ (L Unrelate {}) ctx = pure $ () <$ ctx
  -- FIXME
  alg _ (L GetRelated {}) ctx = pure $ bool Nothing (Just undefined) (fromSBool $ sbool @b) <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
