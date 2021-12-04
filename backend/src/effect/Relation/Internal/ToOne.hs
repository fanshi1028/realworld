{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ToOne Relation
--
-- @since 0.3.0.0
module Relation.Internal.ToOne where

import GHC.Base (Symbol)

class ToOne (r :: Symbol) where
  type ToOneKey r
  type ToOneValue r
