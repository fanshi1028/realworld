{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.0.0
module Client.Internal.Orphans.ToJSON where

import Data.Aeson (Key, ToJSON (toEncoding, toJSON), Value (Object))
import qualified Data.Aeson.KeyMap as KM (insert)
import Data.Authentication.HasAuth (HasAuth (LoginOf))
import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Password (Password (Password))
import Data.Generics.Product (field')
import Data.Password.Argon2 (unsafeShowPassword)
import qualified Data.Semigroup as SG (Last (getLast))
import Data.Storage.Map.HasCreate (CreateOf)
import Data.Storage.Map.HasUpdate (Patch, UpdateOf)
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (Out), wrappedToJSON)
import Data.Util.Validation (WithValidation)
import Relude.Extra ((^.))
import Validation (Validation (Failure, Success))

-- | @since 0.4.0.0
wrappedToJSON' :: ToJSON a => Key -> In a -> Value
wrappedToJSON' key (In a) = wrappedToJSON key $ Out a

-- | @since 0.4.0.0
instance ToJSON a => ToJSON (WithValidation a) where
  toJSON (Failure err) = error $ "Invalidated toJSON: " <> show err
  toJSON (Success a) = toJSON a
  toEncoding (Failure err) = error $ "Invalidated toJSON: " <> show err
  toEncoding (Success a) = toEncoding a

-- | @since 0.4.0.0
instance ToJSON (In a) => ToJSON (In (WithValidation a)) where
  toJSON (In (Failure err)) = error $ "Invalidated toJSON: " <> show err
  toJSON (In (Success a)) = toJSON $ In a
  toEncoding (In (Failure err)) = error $ "Invalidated toJSON: " <> show err
  toEncoding (In (Success a)) = toEncoding $ In a

-- | @since 0.4.0.0
instance ToJSON (LoginOf 'User)

-- | @since 0.4.0.0
instance ToJSON (In (LoginOf 'User)) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (In (Patch (UpdateOf 'Article))) where
  toJSON = wrappedToJSON' "article"

-- | @since 0.4.0.0
instance ToJSON (CreateOf 'User)

-- | @since 0.4.0.0
instance ToJSON (In (CreateOf 'User)) where
  toJSON = wrappedToJSON' "user"

-- | @since 0.4.0.0
instance ToJSON (CreateOf 'Article)

-- | @since 0.4.0.0
instance ToJSON (In (CreateOf 'Article)) where
  toJSON = wrappedToJSON' "article"

-- | @since 0.4.0.0
instance ToJSON (CreateOf 'Comment)

-- | @since 0.4.0.0
instance ToJSON (In (CreateOf 'Comment)) where
  toJSON = wrappedToJSON' "comment"

-- | @since 0.4.0.0
-- HACK
instance ToJSON Password where
  toJSON (Password pw) = toJSON $ unsafeShowPassword pw
  toEncoding (Password pw) = toEncoding $ unsafeShowPassword pw

-- | @since 0.4.0.0
instance ToJSON (Patch (UpdateOf 'User)) where
  toJSON a =
    let insert' key = KM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "email") $ a ^. field' @"email"
        h2 = mayDo (insert' "username") $ a ^. field' @"username"
        h3 = mayDo (insert' "password") $ a ^. field' @"password"
        h4 = mayDo (insert' "bio") $ a ^. field' @"bio"
        h5 = mayDo (insert' "image") $ a ^. field' @"image"
     in Object $ h1 $ h2 $ h3 $ h4 $ h5 mempty

-- | @since 0.4.0.0
instance ToJSON (In (Patch (UpdateOf 'User))) where
  toJSON = wrappedToJSON' "user"

-- | @since 0.4.0.0
instance ToJSON (Patch (UpdateOf 'Article)) where
  toJSON a =
    let insert' key = KM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "title") $ a ^. field' @"title"
        h2 = mayDo (insert' "description") $ a ^. field' @"description"
        h3 = mayDo (insert' "body") $ a ^. field' @"body"
     in Object $ h1 $ h2 $ h3 mempty
