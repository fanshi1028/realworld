-- |
-- Description : Typeclass & Instances
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ToMany Relation
--
-- @since 0.3.0.0
module Relation.ToMany
  ( -- * Typeclass
    module X,
  )
where

import Relation.Internal.ToMany as X
import Relation.Internal.ToMany.ArticleHasComment as X ()
import Relation.Internal.ToMany.UserCreateArticle as X ()
import Relation.Internal.ToMany.UserCreateComment as X ()
