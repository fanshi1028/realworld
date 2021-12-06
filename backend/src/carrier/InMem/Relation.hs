-- |
-- Description : Typeclass & Instances
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ManyToMany Relation
--
-- @since 0.3.0.0
module InMem.Relation
  ( -- * Typeclass

    -- ** ToOne
    module X,

    -- ** ToMany
    module Y,

    -- ** ManyToMany
    module Z,
  )
where

import InMem.Relation.Internal.ManyToMany as Z
import InMem.Relation.Internal.ManyToMany.Favorite as Z ()
import InMem.Relation.Internal.ManyToMany.Follow as Z ()
import InMem.Relation.Internal.ManyToMany.Tag as Z ()
import InMem.Relation.Internal.ToMany as Y
import InMem.Relation.Internal.ToMany.ArticleHasComment as Y ()
import InMem.Relation.Internal.ToMany.UserCreateArticle as Y ()
import InMem.Relation.Internal.ToMany.UserCreateComment as Y ()
import InMem.Relation.Internal.ToOne as X
import InMem.Relation.Internal.ToOne.EmailOfUser as X ()
