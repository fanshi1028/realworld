{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
module Client.Internal.Orphans.FromJSON where

import Data.Aeson (FromJSON (parseJSON), Value (Object, String), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile (ArticleWithAuthorProfile))
import Data.Domain.Comment (CommentWithAuthorProfile (..))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken), UserProfile (UserProfile))
import Data.Field.Slug (Slug (Slug))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Username (Username (Username))
import Data.Generics.Product.Fields (getField)
import Data.HashMap.Strict (insert)
import Data.Storage.Map (ContentOf (..), IdOf (..))
import Data.Token.HasToken (TokenOf (UserToken))
import Data.Util.JSON.From (In (In), wrappedParseJSON)
import Data.Util.JSON.To (Out (Out))

wrappedParseJSON' :: FromJSON a => String -> Text -> Value -> Parser (Out a)
wrappedParseJSON' info key = wrappedParseJSON info key >=> \(In a) -> pure (Out a)

deriving newtype instance FromJSON Tag

deriving newtype instance FromJSON Slug

deriving newtype instance FromJSON (IdOf 'User)

deriving newtype instance FromJSON (IdOf 'Article)

instance FromJSON (ContentOf 'Article)

instance FromJSON UserProfile where
  parseJSON v = withObject "UserProfile" (\o -> UserProfile <$> parseJSON v <*> o .: "following") v

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

instance FromJSON (Out ArticleWithAuthorProfile) where
  parseJSON = withObject "out ArticleWithAuthorProfile" $ \o -> Out <$> o .: "article"

instance FromJSON (Out [ArticleWithAuthorProfile]) where
  parseJSON = withObject "Out [ ArticleWithAuthorProfile ]" $ \o -> Out <$> o .: "articles"

deriving newtype instance FromJSON (IdOf 'Comment)

instance FromJSON CommentWithAuthorProfile

instance FromJSON (Out CommentWithAuthorProfile) where
  parseJSON = withObject "Out CommentWithAuthorProfile" $ \o -> Out <$> o .: "comment"

instance FromJSON (Out [CommentWithAuthorProfile]) where
  parseJSON = withObject "Out [ CommentWithAuthorProfile ]" $ \o -> Out <$> o .: "comments"

instance FromJSON (Out [Tag]) where
  parseJSON = withObject "Out [ Tag ]" $ \o -> Out <$> o .: "tags"

instance FromJSON (TokenOf 'User) where
  parseJSON = UserToken . encodeUtf8 <<$>> parseJSON @Text

instance FromJSON UserAuthWithToken where
  parseJSON v = withObject "UserAuthWithToken" (\o -> UserAuthWithToken <$> parseJSON v <*> o .: "token") v

instance FromJSON (Out UserAuthWithToken) where
  parseJSON = withObject "Out UserAuthWithToken" $ \o -> Out <$> o .: "user"

instance FromJSON (Out UserProfile) where
  parseJSON = withObject "Out UserProfile" $ \o -> Out <$> o .: "profile"
