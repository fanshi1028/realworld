{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Has Tag Relation
--
-- @since 0.3.0.0
module Relation.Internal.ManyToMany.Tag where

import Relation.Internal.ManyToMany (ManyToMany (..))
import Relation.ToMany (ToMany(..))
import Field.Tag (Tag)
import Storage.Map (IdOf)
import Domain (Domain(Article))

-- | @since 0.3.0.0
instance ToMany "ArticleTaggedByTag" where
  type ToManyKey "ArticleTaggedByTag" = IdOf 'Article
  type ToManyValue "ArticleTaggedByTag" = Tag

-- | @since 0.3.0.0
instance ToMany "TagTagArticle" where
  type ToManyKey "TagTagArticle" = Tag
  type ToManyValue "TagTagArticle" = IdOf 'Article

-- | @since 0.3.0.0
instance ManyToMany "ArticleTaggedByTag" where
  type ManyLeft "ArticleTaggedByTag" = "ArticleTaggedByTag"
  type ManyRight "ArticleTaggedByTag" = "TagTagArticle"
