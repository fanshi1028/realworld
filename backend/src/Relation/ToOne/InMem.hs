{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.ToOne.InMem (ExistAction (..), run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import qualified Focus as FC (Change (Leave, Remove, Set), unitCases)
import GHC.TypeLits (Symbol)
import Relation.ToOne (E (GetRelated, IsRelated, Relate, Unrelate))
import qualified StmContainers.Map as STM (Map, focus, lookup)

data ExistAction = UpdateIfExist | IgnoreIfExist

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (ex :: ExistAction)
    (m :: Type -> Type)
    a = C
  { run' :: ReaderT (STM.Map r1 r2) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Map r1 r2))

relateFocus :: (Eq key, Hashable key) => ExistAction -> key -> value -> STM.Map key value -> STM ()
relateFocus existAction k v =
  STM.focus
    ( FC.unitCases
        (FC.Set v)
        $ const $
          case existAction of
            UpdateIfExist -> FC.Set v
            IgnoreIfExist -> FC.Leave
    )
    k
{-# INLINE relateFocus #-}

unrelateFocus :: (Hashable key, Eq key, Eq value) => key -> value -> STM.Map key value -> STM ()
unrelateFocus k v = STM.focus (FC.unitCases FC.Leave (\e -> if v == e then FC.Remove else FC.Leave)) k
{-# INLINE unrelateFocus #-}

instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 'IgnoreIfExist m)
  where
  alg _ (L action) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . case action of
        Relate r1 r2 -> relateFocus IgnoreIfExist r1 r2
        Unrelate r1 r2 -> unrelateFocus r1 r2
        IsRelated r1 r2 -> (== Just r2) <<$>> STM.lookup r1
        GetRelated r1 -> STM.lookup r1
  alg hdl (R other) ctx = C $ alg (run' . hdl) (R other) ctx
  {-# INLINE alg #-}

instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 'UpdateIfExist m)
  where
  alg _ (L action) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . case action of
        Relate r1 r2 -> relateFocus UpdateIfExist r1 r2
        Unrelate r1 r2 -> unrelateFocus r1 r2
        IsRelated r1 r2 -> (== Just r2) <<$>> STM.lookup r1
        GetRelated r1 -> STM.lookup r1
  alg hdl (R other) ctx = C $ alg (run' . hdl) (R other) ctx
  {-# INLINE alg #-}

run :: forall r1 r r2 ex a m. STM.Map r1 r2 -> C r1 r r2 ex m a -> m a
run db = run' >>> usingReaderT db
