{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier to run in memory
--
-- @since 0.1.0.0
module Relation.ManyToMany.InMem where

import qualified Relation.ManyToMany (C (run), ManyLeft, ManyRight)
import qualified Relation.ToMany.InMem (C, run)
import StmContainers.Multimap (Multimap)

-- | @since 0.1.0.0
run ::
  forall r1 r r2 a m lr rr.
  ( lr ~ Relation.ManyToMany.ManyLeft r,
    rr ~ Relation.ManyToMany.ManyRight r
  ) =>
  Multimap r1 r2 ->
  Multimap r2 r1 ->
  Relation.ManyToMany.C r1 r r2 (Relation.ToMany.InMem.C r1 lr r2 (Relation.ToMany.InMem.C r2 rr r1 m)) a ->
  m a
run db1 db2 =
  Relation.ManyToMany.run
    >>> Relation.ToMany.InMem.run db1
    >>> Relation.ToMany.InMem.run db2
