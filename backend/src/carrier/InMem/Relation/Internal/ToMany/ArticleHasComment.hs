{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Article has Comment Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ToMany.ArticleHasComment where

import Domain (Domain (Article, Comment))
import InMem.Relation.Internal.ToMany (ToMany (..))
import Storage.Map (IdOf)

-- | @since 0.3.0.0
data ArticleHasComment

-- | @since 0.3.0.0
instance ToMany ArticleHasComment where
  type ToManyKey ArticleHasComment = IdOf 'Article
  type ToManyValue ArticleHasComment = IdOf 'Comment
