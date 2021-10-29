{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of one to many relation
--
-- @since 0.1.0.0
module Relation.ToMany (E (..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  -- | @since 0.1.0.0
  Relate :: r1 -> r2 -> E r1 r r2 m ()
  -- | @since 0.1.0.0
  Unrelate :: r1 -> r2 -> E r1 r r2 m ()
  -- | @since 0.1.0.0
  -- unrelate every value related to the key
  UnrelateByKey :: r1 -> E r1 r r2 m ()
  -- | @since 0.1.0.0
  IsRelated :: r1 -> r2 -> E r1 r r2 m Bool
  -- | @since 0.1.0.0
  -- get every value related to the key
  GetRelated :: r1 -> E r1 r r2 m [r2]
