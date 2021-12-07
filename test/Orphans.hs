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

import Authentication.HasAuth (AuthOf (..), LoginOf (..))
import Authorization (TokenAuth)
import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON (toEncoding, toJSON), Value (Object, String), parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Generics.Product (field')
import Data.HashMap.Strict (insert)
import qualified Data.HashMap.Strict as HM (insert)
import Data.Password.Argon2 (unsafeShowPassword)
import qualified Data.Semigroup as SG (getLast)
import Data.Sequence ((<|))
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Field.Bio (Bio (Bio))
import Field.Body (Body (Body))
import Field.Description (Description (Description))
import Field.Email (Email (Email))
import Field.Image (Image (Image))
import Field.Password (Password (Password))
import Field.Slug (Slug (Slug))
import Field.Tag (Tag (Tag))
import Field.Title (Title (Title))
import Field.Username (Username (Username))
import GHC.Records (getField)
import HTTP.Util (Limit (Limit), Offset (Offset))
import Network.HTTP.Types (hAuthorization)
import Servant (ToHttpApiData (toUrlPiece), type (:>))
import Servant.Auth.Server (Auth)
import Servant.Client (HasClient (Client, clientWithRoute), hoistClientMonad)
import Servant.Client.Core (requestHeaders)
import Storage.Map (ContentOf, CreateOf (..), IdOf (..), Patch, UpdateOf (..))
import Test.StateMachine (ToExpr (toExpr))
import Token.HasToken (TokenOf (UserToken))
import Util.JSON.From (In (In), wrappedParseJSON)
import Util.JSON.To (Out (Out), wrappedToJSON)
import Util.Validation (WithValidation)
import Validation (Validation (Failure, Success))

class TokenAuthNotEnabled

-- | For supporting TokenAuth client generation's instance
type family HasTokenAuth xs :: Constraint where
  HasTokenAuth '[TokenAuth] = ()
  HasTokenAuth (x ': xs) = HasTokenAuth xs
  HasTokenAuth '[] = TokenAuthNotEnabled

instance (HasTokenAuth auths, HasClient m api) => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = TokenOf 'User -> Client m api
  clientWithRoute m _ req (UserToken t) =
    clientWithRoute m (Proxy :: Proxy api) $
      req
        { requestHeaders =
            (hAuthorization, "Token " <> t) <| requestHeaders req
        }
  hoistClientMonad p _ f cl = hoistClientMonad p (Proxy :: Proxy api) f . cl

wrappedParseJSON' :: FromJSON a => String -> Text -> Value -> Parser (Out a)
wrappedParseJSON' info key = wrappedParseJSON info key >=> \(In a) -> pure (Out a)

wrappedToJSON' :: ToJSON a => Text -> In a -> Value
wrappedToJSON' key (In a) = wrappedToJSON key $ Out a

instance ToHttpApiData a => ToHttpApiData (WithValidation a) where
  toUrlPiece (Failure err) = error $ "Invalid http api data: " <> show err
  toUrlPiece (Success a) = toUrlPiece a

deriving newtype instance ToHttpApiData Username

deriving newtype instance ToHttpApiData (IdOf 'User)

deriving newtype instance ToHttpApiData Slug

deriving newtype instance ToHttpApiData (IdOf 'Article)

deriving newtype instance ToHttpApiData (IdOf 'Comment)

deriving newtype instance ToHttpApiData Tag

deriving newtype instance ToHttpApiData Limit

deriving newtype instance ToHttpApiData Offset

deriving newtype instance FromJSON Tag

deriving newtype instance FromJSON Slug

deriving newtype instance FromJSON (IdOf 'User)

deriving newtype instance FromJSON (IdOf 'Article)

instance FromJSON (ContentOf 'Article)

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

deriving newtype instance FromJSON (IdOf 'Comment)

instance FromJSON (CommentR "withAuthorProfile")

instance FromJSON (Out (CommentR "withAuthorProfile")) where
  parseJSON = withObject "Out CommentR withAuthorProfile" $ \o -> Out <$> o .: "comment"

instance FromJSON (Out [CommentR "withAuthorProfile"]) where
  parseJSON = withObject "Out [ CommentR withAuthorProfile ]" $ \o -> Out <$> o .: "comments"

instance FromJSON (Out [Tag]) where
  parseJSON = withObject "Out [ Tag ]" $ \o -> Out <$> o .: "tags"

instance FromJSON (TokenOf 'User) where
  parseJSON = UserToken . encodeUtf8 <<$>> parseJSON @Text

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
  toJSON (Password pw) = toJSON $ unsafeShowPassword pw
  toEncoding (Password pw) = toEncoding $ unsafeShowPassword pw

instance ToJSON (LoginOf 'User)

deriving newtype instance Eq a => Eq (In a)

-- instance ToJSON (In (WithValidation (LoginOf 'User)))
instance ToJSON (In (LoginOf 'User)) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (CreateOf 'User)

instance ToJSON (In (CreateOf 'User)) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (CreateOf 'Article)

instance ToJSON (In (CreateOf 'Article)) where
  toJSON = wrappedToJSON' "article"

instance ToJSON (CreateOf 'Comment)

instance ToJSON (In (CreateOf 'Comment)) where
  toJSON = wrappedToJSON' "comment"

instance ToJSON (Patch (UpdateOf 'User)) where
  toJSON a =
    let insert' key = HM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "email") $ a ^. field' @"email"
        h2 = mayDo (insert' "username") $ a ^. field' @"username"
        h3 = mayDo (insert' "password") $ a ^. field' @"password"
        h4 = mayDo (insert' "bio") $ a ^. field' @"bio"
        h5 = mayDo (insert' "image") $ a ^. field' @"image"
     in Object $ h1 $ h2 $ h3 $ h4 $ h5 mempty

instance ToJSON (In (Patch (UpdateOf 'User))) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (Patch (UpdateOf 'Article)) where
  toJSON a =
    let insert' key = HM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "title") $ a ^. field' @"title"
        h2 = mayDo (insert' "description") $ a ^. field' @"description"
        h3 = mayDo (insert' "body") $ a ^. field' @"body"
     in Object $ h1 $ h2 $ h3 mempty

instance ToJSON (In (Patch (UpdateOf 'Article))) where
  toJSON = wrappedToJSON' "article"

-- toEncoding  = wrappedToEncoding' 'Article

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
