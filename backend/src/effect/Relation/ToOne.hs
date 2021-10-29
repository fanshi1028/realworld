{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of one to one relation
--
-- @since 0.1.0.0
module Relation.ToOne (E (..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.2.0.0
data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  -- | @since 0.2.0.0
  Relate :: r1 -> r2 -> E r1 r r2 m ()
  -- | @since 0.2.0.0
  Unrelate :: r1 -> r2 -> E r1 r r2 m ()
  -- | @since 0.2.0.0
  GetRelated :: r1 -> E r1 r r2 m (Maybe r2)
