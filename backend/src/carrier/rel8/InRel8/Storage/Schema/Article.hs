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
-- Schema for article
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.Article where

import Domain (Domain (Article, User))
import Field.Body (Body)
import Field.Description (Description)
import Field.Time (Time)
import Field.Title (Title)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))
import Storage.Map (IdOf)

-- | @since 0.4.0.0
data ArticleRel8 f = ArticleRel8
  { slug :: Column f (IdOf 'Article),
    title :: Column f Title,
    description :: Column f Description,
    body :: Column f Body,
    author :: Column f (IdOf 'User),
    createdAt :: Column f Time,
    updatedAt :: Column f Time
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able ArticleRel8

-- | @since 0.4.0.0
deriving instance Show (ArticleRel8 Result)

-- | @since 0.4.0.0
articleSchema :: TableSchema (ArticleRel8 Name)
articleSchema = TableSchema "articles" Nothing $ snakeNamesFromLabels @(ArticleRel8 Name)