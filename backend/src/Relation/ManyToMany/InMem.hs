{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.ManyToMany.InMem where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated, Relate, Unrelate, UnrelateByKeyLeft, UnrelateByKeyRight))
import qualified Relation.OneToMany (E (Relate, Unrelate, GetRelated, UnrelateByKey, IsRelated))

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (m :: Type -> Type)
    a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Member (Relation.OneToMany.E r1 r r2) sig,
    Member (Relation.OneToMany.E r2 r r1) sig
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      Relate r1 r2 -> do
        send $ Relation.OneToMany.Relate @_ @_ @r r1 r2
        send $ Relation.OneToMany.Relate @_ @_ @r r2 r1
      Unrelate r1 r2 -> do
        send $ Relation.OneToMany.Unrelate @_ @_ @r r1 r2
        send $ Relation.OneToMany.Unrelate @_ @_ @r r2 r1
      UnrelateByKeyLeft r1 -> void $
        runNonDetA @[] $ do
          send (Relation.OneToMany.GetRelated @_ @r @r2 r1)
            >>= oneOf
            >>= send . \r2 -> Relation.OneToMany.Unrelate @_ @_ @r r2 r1
          send $ Relation.OneToMany.UnrelateByKey @_ @r @r2 r1
      UnrelateByKeyRight r2 -> void $
        runNonDetA @[] $ do
          send (Relation.OneToMany.GetRelated @_ @r @r1 r2)
            >>= oneOf
            >>= send . \r1 -> Relation.OneToMany.Unrelate @_ @_ @r r1 r2
          send $ Relation.OneToMany.UnrelateByKey @_ @r @r1 r2
      IsRelated r1 r2 -> send $ Relation.OneToMany.IsRelated @_ @_ @r r1 r2
      GetRelatedLeft r1 -> send $ Relation.OneToMany.GetRelated @_ @r @r2 r1
      GetRelatedRight r2 -> send $ Relation.OneToMany.GetRelated @_ @r @r1 r2
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
