{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.0.0
module Client.Internal.Orphans.FromJSON where

import Data.Aeson (FromJSON (parseJSON), Key, Value (Object, String), withObject, (.:))
import Data.Aeson.KeyMap (insert)
import Data.Aeson.Types (Parser)
import Data.Authentication.HasToken (TokenOf (UserToken))
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile (ArticleWithAuthorProfile))
import Data.Domain.Comment (CommentWithAuthorProfile (..))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken), UserProfile (UserProfile))
import Data.Field.Slug (Slug (Slug))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Username (Username (Username))
import Data.Generics.Product.Fields (getField)
import Data.Storage.Map.HasStorage (ContentOf (..), IdOf (..))
import Data.Util.JSON.From (In (In), wrappedParseJSON)
import Data.Util.JSON.To (Out (Out))

-- | @since 0.4.0.0
wrappedParseJSON' :: FromJSON a => String -> Key -> Value -> Parser (Out a)
wrappedParseJSON' info key = wrappedParseJSON info key >=> \(In a) -> pure (Out a)

-- | @since 0.4.0.0
deriving newtype instance FromJSON Tag

-- | @since 0.4.0.0
deriving newtype instance FromJSON Slug

-- | @since 0.4.0.0
deriving newtype instance FromJSON (IdOf 'User)

-- | @since 0.4.0.0
deriving newtype instance FromJSON (IdOf 'Article)

-- | @since 0.4.0.0
instance FromJSON (ContentOf 'Article)

-- | @since 0.4.0.0
instance FromJSON UserProfile where
  parseJSON v = withObject "UserProfile" (\o -> UserProfile <$> parseJSON v <*> o .: "following") v

-- | @since 0.4.0.0
instance FromJSON ArticleWithAuthorProfile where
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
          ?? a

-- | @since 0.4.0.0
instance FromJSON (Out ArticleWithAuthorProfile) where
  parseJSON = withObject "out ArticleWithAuthorProfile" $ \o -> Out <$> o .: "article"

-- | @since 0.4.0.0
instance FromJSON (Out [ArticleWithAuthorProfile]) where
  parseJSON = withObject "Out [ ArticleWithAuthorProfile ]" $ \o -> Out <$> o .: "articles"

-- | @since 0.4.0.0
deriving newtype instance FromJSON (IdOf 'Comment)

-- | @since 0.4.0.0
instance FromJSON CommentWithAuthorProfile

-- | @since 0.4.0.0
instance FromJSON (Out CommentWithAuthorProfile) where
  parseJSON = withObject "Out CommentWithAuthorProfile" $ \o -> Out <$> o .: "comment"

-- | @since 0.4.0.0
instance FromJSON (Out [CommentWithAuthorProfile]) where
  parseJSON = withObject "Out [ CommentWithAuthorProfile ]" $ \o -> Out <$> o .: "comments"

-- | @since 0.4.0.0
instance FromJSON (Out [Tag]) where
  parseJSON = withObject "Out [ Tag ]" $ \o -> Out <$> o .: "tags"

-- | @since 0.4.0.0
instance FromJSON (TokenOf 'User) where
  parseJSON = UserToken . encodeUtf8 <<$>> parseJSON @Text

-- | @since 0.4.0.0
instance FromJSON UserAuthWithToken where
  parseJSON v = withObject "UserAuthWithToken" (\o -> UserAuthWithToken <$> parseJSON v <*> o .: "token") v

-- | @since 0.4.0.0
instance FromJSON (Out UserAuthWithToken) where
  parseJSON = withObject "Out UserAuthWithToken" $ \o -> Out <$> o .: "user"

-- | @since 0.4.0.0
instance FromJSON (Out UserProfile) where
  parseJSON = withObject "Out UserProfile" $ \o -> Out <$> o .: "profile"
