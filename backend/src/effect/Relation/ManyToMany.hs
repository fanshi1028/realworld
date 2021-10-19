{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of many to many relation
--
-- @since 0.1.0.0
module Relation.ManyToMany
  ( -- * Type families

    -- | We model many to many relation by two opposite on to many relations.
    -- Hence two type families storing the tag of the corresponding relations.
    ManyLeft,
    ManyRight,

    -- * Effect
    E (..),

    -- * Carrier
    C (..),
  )
where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import qualified Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))

-- | @since 0.1.0.0
type family ManyLeft (many :: Symbol) :: Symbol

-- | @since 0.1.0.0
type family ManyRight (many :: Symbol) :: Symbol

-- follow

-- | @since 0.1.0.0
type instance ManyLeft "follow" = "following"

-- | @since 0.1.0.0
type instance ManyRight "follow" = "followedBy"

-- favorite

-- | @since 0.1.0.0
type instance ManyLeft "favorite" = "favorite"

-- | @since 0.1.0.0
type instance ManyRight "favorite" = "favoritedBy"

-- taggedBy

-- | @since 0.1.0.0
type instance ManyLeft "taggedBy" = "taggedBy"

-- | @since 0.1.0.0
type instance ManyRight "taggedBy" = "tagging"

-- | @since 0.1.0.0
data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  Relate :: r1 -> r2 -> E r1 r r2 m ()
  Unrelate :: r1 -> r2 -> E r1 r r2 m ()
  -- | Unrelate every r2 related to the key r1
  UnrelateByKeyLeft :: r1 -> E r1 r r2 m ()
  -- | Unrelate every r1 related to the key r2
  UnrelateByKeyRight :: r2 -> E r1 r r2 m ()
  IsRelated :: r1 -> r2 -> E r1 r r2 m Bool
  -- | Get every r2 related to the key r1
  GetRelatedLeft :: r1 -> E r1 r r2 m [r2]
  -- | Get every r1 related to the key r2
  GetRelatedRight :: r2 -> E r1 r r2 m [r1]

-- | @since 0.1.0.0
newtype C (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | Work are forworded to the two __one to many__ relations
--
-- @since 0.1.0.0
instance
  ( Algebra sig m,
    lr ~ ManyLeft r,
    rr ~ ManyRight r,
    Member (Relation.ToMany.E r1 lr r2) sig,
    Member (Relation.ToMany.E r2 rr r1) sig
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      Relate r1 r2 -> do
        send $ Relation.ToMany.Relate @_ @_ @lr r1 r2
        send $ Relation.ToMany.Relate @_ @_ @rr r2 r1
      Unrelate r1 r2 -> do
        send $ Relation.ToMany.Unrelate @_ @_ @lr r1 r2
        send $ Relation.ToMany.Unrelate @_ @_ @rr r2 r1
      UnrelateByKeyLeft r1 -> void $
        runNonDetA @[] $ do
          send (Relation.ToMany.GetRelated @_ @lr @r2 r1)
            >>= oneOf
            >>= send . \r2 -> Relation.ToMany.Unrelate @_ @_ @rr r2 r1
          send $ Relation.ToMany.UnrelateByKey @_ @lr @r2 r1
      UnrelateByKeyRight r2 -> void $
        runNonDetA @[] $ do
          send (Relation.ToMany.GetRelated @_ @rr @r1 r2)
            >>= oneOf
            >>= send . \r1 -> Relation.ToMany.Unrelate @_ @_ @lr r1 r2
          send $ Relation.ToMany.UnrelateByKey @_ @rr @r1 r2
      IsRelated r1 r2 -> send $ Relation.ToMany.IsRelated @_ @_ @lr r1 r2
      GetRelatedLeft r1 -> send $ Relation.ToMany.GetRelated @_ @lr @r2 r1
      GetRelatedRight r2 -> send $ Relation.ToMany.GetRelated @_ @rr @r1 r2
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
