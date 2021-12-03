{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of visitors' action
--
-- @since 0.3.0.0
module VisitorAction where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Field.Tag (Tag)
import qualified StmContainers.Set as STMSet (Set)
import Storage.InMem (setInMemGetAll)

-- * Effect

-- | @since 0.3.0.0
-- Actions that can be carried out by visitor(__unauthenticated__).
data VisitorActionE (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Get all the tags.
  GetTags :: VisitorActionE m [Tag]

-- * Carrirer

-- | @since 0.3.0.0
newtype VisitorActionInMemC m a = VisitorActionInMemC
  { -- | @since 0.3.0.0
    runVisitorActionInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Member (R.Reader (STMSet.Set Tag)) sig
  ) =>
  Algebra (VisitorActionE :+: sig) (VisitorActionInMemC m)
  where
  alg _ (L GetTags) ctx = (<$ ctx) <$> setInMemGetAll @_ @_ @Tag
  alg hdl (R other) ctx = VisitorActionInMemC $ alg (runVisitorActionInMem . hdl) other ctx
  {-# INLINE alg #-}
