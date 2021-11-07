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
-- @since 0.1.0.0
module VisitorAction where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Field.Tag (Tag)
import qualified Storage.Set (E (GetAll))

-- * Effect

-- | @since 0.3.0.0
-- Actions that can be carried out by visitor(__unauthenticated__).
data E (m :: Type -> Type) a where
  -- | @since 0.1.0.0
  -- Get all the tags.
  GetTags :: E m [Tag]

-- * Carrirer

-- | @since 0.1.0.0
newtype C m a = C
  { -- | @since 0.1.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( Member (Storage.Set.E Tag) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L GetTags) ctx = (<$ ctx) <$> send (Storage.Set.GetAll @Tag)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
