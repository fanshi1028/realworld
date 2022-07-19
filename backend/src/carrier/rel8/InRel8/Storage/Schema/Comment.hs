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
-- Schema for comment
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.Comment where

import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Time (Time)
import Data.Storage.Map.HasStorage (IdOf)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))

-- | @since 0.4.0.0
data CommentRel8 f = CommentRel8
  { id :: Column f (IdOf 'Comment),
    body :: Column f Text, -- "It takes a Jacobian",
    author :: Column f (IdOf 'User),
    article :: Column f (IdOf 'Article),
    createdAt :: Column f Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Column f Time -- "2016-02-18T03:22:56.637Z"
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able CommentRel8

-- | @since 0.4.0.0
deriving instance Show (CommentRel8 Result)

-- | @since 0.4.0.0
commentSchema :: TableSchema (CommentRel8 Name)
commentSchema = TableSchema "comments" Nothing $ snakeNamesFromLabels @(CommentRel8 Name)
