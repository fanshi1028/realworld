{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Has Tag Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ManyToMany.Tag where

import Domain (Domain (Article))
import Field.Tag (Tag)
import InMem.Relation.Internal.ManyToMany (ManyToMany (..))
import InMem.Relation.Internal.ToMany (ToMany (..))
import Storage.Map (IdOf)

-- | @since 0.3.0.0
data ArticleTaggedByTag

-- | @since 0.3.0.0
data TagTagArticle

-- | @since 0.3.0.0
instance ToMany ArticleTaggedByTag where
  type ToManyKey ArticleTaggedByTag = IdOf 'Article
  type ToManyValue ArticleTaggedByTag = Tag

-- | @since 0.3.0.0
instance ToMany TagTagArticle where
  type ToManyKey TagTagArticle = Tag
  type ToManyValue TagTagArticle = IdOf 'Article

-- | @since 0.3.0.0
instance ManyToMany ArticleTaggedByTag where
  type ManyLeft ArticleTaggedByTag = ArticleTaggedByTag
  type ManyRight ArticleTaggedByTag = TagTagArticle
