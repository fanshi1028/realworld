-- |
-- Description : Typeclass & Instances
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ManyToMany Relation
--
-- @since 0.3.0.0
module Relation.ManyToMany
  ( -- * Typeclass
    module X,
  )
where

import Relation.Internal.ManyToMany as X
import Relation.Internal.ManyToMany.Favorite as X ()
import Relation.Internal.ManyToMany.Follow as X ()
import Relation.Internal.ManyToMany.Tag as X ()
