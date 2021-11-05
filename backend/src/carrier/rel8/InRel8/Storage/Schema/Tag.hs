{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Schema
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Schema for tag
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.Tag where

import Field.Tag (Tag)
import Field.Time (Time)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))

-- | @since 0.4.0.0
data TagRel8 f = TagRel8
  { tag :: Column f Tag,
    createdAt :: Column f Time -- "2016-02-18T03:22:56.637Z",
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able TagRel8

-- | @since 0.4.0.0
deriving instance Show (TagRel8 Result)

-- | @since 0.4.0.0
tagSchema :: TableSchema (TagRel8 Name)
tagSchema = TableSchema "tags" Nothing $ snakeNamesFromLabels @(TagRel8 Name)
