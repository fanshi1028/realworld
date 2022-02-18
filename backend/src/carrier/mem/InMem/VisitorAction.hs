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
-- Carrier of visitors' action in memory
--
-- @since 0.3.0.0
module InMem.VisitorAction where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import InMem.Relation (ArticleTaggedByTag, ManyToMany (getAllRightManyToMany), ManyToManyRelationE)
import VisitorAction (VisitorActionE (GetTags))

-- | @since 0.3.0.0
newtype VisitorActionInMemC (f :: Type -> Type) m a = VisitorActionInMemC
  { -- | @since 0.3.0.0
    runVisitorActionInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    ManyToManyRelationE ArticleTaggedByTag sig
  ) =>
  Algebra (VisitorActionE [] :+: sig) (VisitorActionInMemC [] m)
  where
  alg _ (L GetTags) ctx = (<$ ctx) <$> getAllRightManyToMany @ArticleTaggedByTag
  alg hdl (R other) ctx = VisitorActionInMemC $ alg (runVisitorActionInMem . hdl) other ctx
  {-# INLINE alg #-}
