{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Schema
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Schema for article_has_tag relation
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.ArticleHasTag where

import Data.Domain (Domain (Article))
import Data.Field.Tag (Tag)
import Data.Field.Time (Time)
import Data.Storage.Map.HasStorage (IdOf)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))

-- | @since 0.4.0.0
data ArticleHasTagRel8 f = ArticleHasTagRel8
  { article :: Column f (IdOf 'Article),
    tag :: Column f Tag,
    createdAt :: Column f Time -- "2016-02-18T03:22:56.637Z",
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able ArticleHasTagRel8

-- | @since 0.4.0.0
deriving instance Show (ArticleHasTagRel8 Result)

-- | @since 0.4.0.0
articleHasTagSchema :: TableSchema (ArticleHasTagRel8 Name)
articleHasTagSchema = TableSchema "article_has_tag" Nothing $ snakeNamesFromLabels @(ArticleHasTagRel8 Name)
