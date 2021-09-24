{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.2.0.0
module Orphans where

import Authorization (TokenAuth)
import Data.Aeson (FromJSON, ToJSON (toEncoding, toJSON), Value (Object, String), parseJSON, withObject, (.:))
import Data.HashMap.Strict (insert)
import Data.Password.Argon2 (Password, unsafeShowPassword)
import Data.Sequence ((<|))
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Field (Slug (Slug), Tag (Tag), Username (Username))
import Domain.Util.JSON.From (In (In), wrappedParseJSON)
import Domain.Util.JSON.To (Out (Out), wrappedToJSON)
import Domain.Util.Validation (WithValidation)
import GHC.Records (getField)
import HTTP.Util (Limit (Limit), Offset (Offset))
import Servant (ToHttpApiData (toUrlPiece))
import Validation (Validation (Failure, Success))
import Data.Aeson.Types (Parser)

wrappedParseJSON' :: FromJSON a => String -> Text -> Value -> Parser (Out a)
wrappedParseJSON' info key = wrappedParseJSON info key >=> \(In a) -> pure (Out a)

wrappedToJSON' :: ToJSON a => Text -> In a -> Value
wrappedToJSON' key (In a) = wrappedToJSON key $ Out a

instance ToHttpApiData a => ToHttpApiData (WithValidation a) where
  toUrlPiece (Failure err) = error $ "Invalid http api data: " <> show err
  toUrlPiece (Success a) = toUrlPiece a

deriving newtype instance ToHttpApiData Username

deriving newtype instance ToHttpApiData (UserR "id")

deriving newtype instance ToHttpApiData Slug

deriving newtype instance ToHttpApiData (ArticleR "id")

deriving newtype instance ToHttpApiData (CommentR "id")

deriving newtype instance ToHttpApiData Tag

deriving newtype instance ToHttpApiData Limit

deriving newtype instance ToHttpApiData Offset

deriving newtype instance FromJSON Tag

deriving newtype instance FromJSON Slug

deriving newtype instance FromJSON (ArticleR "id")

instance FromJSON (ArticleR "all")

instance FromJSON (UserR "profile") where
  parseJSON v = withObject "UserR profile" (\o -> UserProfile <$> parseJSON v <*> o .: "following") v

instance FromJSON (ArticleR "withAuthorProfile") where
  parseJSON =
    withObject
      "withAuthorProfile"
      $ \o -> do
        a <- o .: "author"
        let Username uid = getField @"username" $ getField @"profile" a
        ArticleWithAuthorProfile
          <$> parseJSON (Object $ insert "author" (String uid) o)
          <*> o .: "tagList"
          <*> o .: "favorited"
          <*> o .: "favoritesCount"
          <*> pure a

instance FromJSON (Out (ArticleR "withAuthorProfile")) where
  parseJSON = withObject "out ArticleR withAuthorProfile" $ \o -> Out <$> o .: "article"

instance FromJSON (Out [ArticleR "withAuthorProfile"]) where
  parseJSON = withObject "Out [ ArticleR withAuthorProfile ]" $ \o -> Out <$> o .: "articles"

deriving newtype instance FromJSON (CommentR "id")

instance FromJSON (CommentR "withAuthorProfile")

instance FromJSON (Out (CommentR "withAuthorProfile")) where
  parseJSON = withObject "Out CommentR withAuthorProfile" $ \o -> Out <$> o .: "comment"

instance FromJSON (Out [CommentR "withAuthorProfile"]) where
  parseJSON = withObject "Out [ CommentR withAuthorProfile ]" $ \o -> Out <$> o .: "comments"

instance FromJSON (Out [Tag]) where
  parseJSON = withObject "Out [ Tag ]" $ \o -> Out <$> o .: "tags"

instance FromJSON (UserR "token")

instance FromJSON (UserR "authWithToken") where
  parseJSON v = withObject "UserR authWithToken" (\o -> UserAuthWithToken <$> parseJSON v <*> o .: "token") v

instance FromJSON (Out (UserR "authWithToken")) where
  parseJSON = withObject "Out UserR authWithToken" $ \o -> Out <$> o .: "user"

instance FromJSON (Out (UserR "profile")) where
  parseJSON = withObject "Out UserR profile" $ \o -> Out <$> o .: "profile"

instance ToJSON a => ToJSON (WithValidation a) where
  toJSON (Failure err) = error $ "Invalidated toJSON: " <> show err
  toJSON (Success a) = toJSON a
  toEncoding (Failure err) = error $ "Invalidated toJSON: " <> show err
  toEncoding (Success a) = toEncoding a

instance ToJSON (In a) => ToJSON (In (WithValidation a)) where
  toJSON (In (Failure err)) = error $ "Invalidated toJSON: " <> show err
  toJSON (In (Success a)) = toJSON $ In a
  toEncoding (In (Failure err)) = error $ "Invalidated toJSON: " <> show err
  toEncoding (In (Success a)) = toEncoding $ In a

-- HACK
instance ToJSON Password where
  toJSON = toJSON . unsafeShowPassword
  toEncoding = toEncoding . unsafeShowPassword

instance ToJSON (UserR "login")

deriving newtype instance Eq a => Eq (In a)

-- instance ToJSON (In (WithValidation (UserR "login")))
instance ToJSON (In (UserR "login")) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (UserR "create")

instance ToJSON (In (UserR "create")) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (ArticleR "create")

instance ToJSON (In (ArticleR "create")) where
  toJSON = wrappedToJSON' "article"

instance ToJSON (CommentR "create")

instance ToJSON (In (CommentR "create")) where
  toJSON = wrappedToJSON' "comment"

-- | Password hashed using Argon2
--
-- @since 0.2.0.0
instance Eq Password where
  (==) = (==) `on` unsafeShowPassword

deriving instance Show (ArticleR "update")

deriving instance Eq (UserR "login")

deriving instance Eq (UserR "create")

deriving instance Eq (UserR "update")

-- for state machine, Set in model

deriving instance Ord Username

deriving instance Ord (UserR "id")

deriving instance Ord Slug

deriving instance Ord (ArticleR "id")

deriving instance Ord (CommentR "id")

deriving instance Ord Tag
