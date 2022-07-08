{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of visitors' action in postgres
--
-- @since 0.4.0.0
module InRel8.VisitorAction where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import InRel8.Sql (SqlInRel8E (SqlSelect))
import InRel8.Storage.Schema.Tag (tag, tagSchema)
import Rel8 (each, select)
import VisitorAction (VisitorActionE (GetTags))

-- | @since 0.4.0.0
newtype VisitorActionInRel8C (f :: Type -> Type) m a = VisitorActionInRel8C
  { runVisitorActionInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance (Algebra sig m, Member SqlInRel8E sig) => Algebra (VisitorActionE [] :+: sig) (VisitorActionInRel8C [] m) where
  alg _ (L GetTags) ctx =
    (<$ ctx) <$> do
      send . SqlSelect $ tag <<$>> select (each tagSchema)
  alg hdl (R other) ctx = VisitorActionInRel8C $ alg (runVisitorActionInRel8 . hdl) other ctx
