{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Orphans
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Orphan instances for field in database
--
-- @since 0.4.0.0
module InRel8.Storage.Internal.Field where

import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Bio (Bio (..))
import Data.Field.Body (Body (Body))
import Data.Field.Description (Description (Description))
import Data.Field.Email (Email (..))
import Data.Field.Image (Image (..))
import Data.Field.Password (PasswordHash (..))
import Data.Field.Slug (Slug (Slug))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Time (Time (Time))
import Data.Field.Title (Title (Title))
import Data.Field.Username (Username (Username))
import qualified Data.Password.Argon2 as Argon2 (Argon2, PasswordHash (..))
import Data.Storage.Map.HasStorage (IdOf (..))
import Rel8 (DBEq, DBOrd, DBType)

-- | @since 0.4.0.0
deriving newtype instance DBType Email

-- | @since 0.4.0.0
deriving newtype instance DBEq Email

-- | @since 0.4.0.0
deriving newtype instance DBType Username

-- | @since 0.4.0.0
deriving newtype instance DBEq Username

-- | @since 0.4.0.0
deriving newtype instance DBType (IdOf 'User)

-- | @since 0.4.0.0
deriving newtype instance DBEq (IdOf 'User)

-- | @since 0.4.0.0
deriving newtype instance DBType (Argon2.PasswordHash Argon2.Argon2)

-- | @since 0.4.0.0
deriving newtype instance DBType PasswordHash

-- | @since 0.4.0.0
deriving newtype instance DBEq PasswordHash

-- | @since 0.4.0.0
deriving newtype instance DBType Bio

-- | @since 0.4.0.0
deriving newtype instance DBEq Bio

-- | @since 0.4.0.0
deriving newtype instance DBType Image

-- | @since 0.4.0.0
deriving newtype instance DBEq Image

-- | @since 0.4.0.0
deriving newtype instance DBType Slug

-- | @since 0.4.0.0
deriving newtype instance DBType Title

-- | @since 0.4.0.0
deriving newtype instance DBType (IdOf 'Article)

-- | @since 0.4.0.0
deriving newtype instance DBEq (IdOf 'Article)

-- | @since 0.4.0.0
deriving newtype instance DBType Description

-- | @since 0.4.0.0
deriving newtype instance DBType Body

-- | @since 0.4.0.0
deriving newtype instance DBType Time

-- | @since 0.4.0.0
deriving newtype instance DBEq Time

-- | @since 0.4.0.0
deriving newtype instance DBOrd Time

-- | @since 0.4.0.0
deriving newtype instance DBType (IdOf 'Comment)

-- | @since 0.4.0.0
deriving newtype instance DBEq (IdOf 'Comment)

-- | @since 0.4.0.0
deriving newtype instance DBType Tag

-- | @since 0.4.0.0
deriving newtype instance DBEq Tag
