{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of one to many relation
--
-- @since 0.1.0.0
module Relation.ToMany (E(..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  Relate :: r1 -> r2 -> E r1 r r2 m ()
  Unrelate :: r1 -> r2 -> E r1 r r2 m ()
  -- | unrelate every r2 related to the key r1
  UnrelateByKey :: r1 -> E r1 r r2 m ()
  IsRelated :: r1 -> r2 -> E r1 r r2 m Bool
  GetRelated :: r1 -> E r1 r r2 m [r2]
