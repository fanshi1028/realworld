{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.2.0.0
module Orphans where

import Data.Authentication.HasAuth (AuthOf (..), LoginOf (..))
import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Bio (Bio (Bio))
import Data.Field.Body (Body (Body))
import Data.Field.Description (Description (Description))
import Data.Field.Email (Email (Email))
import Data.Field.Image (Image (Image))
import Data.Field.Password (Password (Password))
import Data.Field.Slug (Slug (Slug))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Title (Title (Title))
import Data.Field.Username (Username (Username))
import Data.Password.Argon2 (unsafeShowPassword)
import Data.Storage.Map (CreateOf (..), IdOf (..), UpdateOf (..))
import Data.Token.HasToken (TokenOf (UserToken))
import Data.Util.JSON.From (In (In))
import Test.StateMachine (ToExpr (toExpr))

deriving newtype instance Eq a => Eq (In a)

-- | Password
--
-- @since 0.2.0.0
instance Eq Password where
  (==) = (==) `on` (\(Password pw) -> unsafeShowPassword pw)

deriving instance Show (UpdateOf 'User)

deriving instance Show (UpdateOf 'Article)

deriving instance Eq (LoginOf 'User)

deriving instance Eq (CreateOf 'User)

deriving instance Eq (CreateOf 'Article)

deriving instance Eq (CreateOf 'Comment)

deriving instance Eq (UpdateOf 'User)

-- for state machine, Set in model

deriving instance Ord Username

deriving instance Ord (IdOf 'User)

deriving instance Ord Slug

deriving instance Ord (IdOf 'Article)

deriving instance Ord (IdOf 'Comment)

deriving instance Ord Tag

deriving newtype instance ToExpr Title

deriving newtype instance ToExpr Description

deriving newtype instance ToExpr Body

deriving newtype instance ToExpr Tag

instance ToExpr (CreateOf 'Article)

deriving newtype instance ToExpr Slug

deriving newtype instance ToExpr (IdOf 'Article)

instance ToExpr (CreateOf 'Comment)

deriving newtype instance ToExpr (IdOf 'Comment)

deriving newtype instance ToExpr Email

deriving newtype instance ToExpr Username

deriving newtype instance ToExpr Bio

deriving newtype instance ToExpr Image

instance ToExpr (AuthOf 'User)

deriving newtype instance ToExpr (IdOf 'User)

instance ToExpr (CreateOf 'User)

instance ToExpr Password where
  toExpr = toExpr . show @Text

instance ToExpr (LoginOf 'User)

deriving newtype instance ToExpr (TokenOf 'User)
