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

import Article (ArticleR (..))
import Authentication (AuthOf (..), LoginOf (..))
import Authorization (TokenAuth)
import Comment (CommentR (..))
import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON (toEncoding, toJSON), Value (Object, String), parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Generics.Product (field')
import Data.HashMap.Strict (insert)
import qualified Data.HashMap.Strict as HM (insert)
import Data.Password.Argon2 (unsafeShowPassword)
import qualified Data.Semigroup as SG (getLast)
import Data.Sequence ((<|))
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
import Servant.Client (HasClient (Client, clientWithRoute))
import Servant.Client.Core (requestHeaders)
import Storage.Map (ContentOf, CreateOf (..), IdOf (..), Patch, UpdateOf (..))
import Test.StateMachine (ToExpr (toExpr))
import Token (TokenOf (UserToken))
import User (UserR (..))
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
  type Client m (Auth auths a :> api) = TokenOf "user" -> Client m api
  clientWithRoute m _ req (UserToken t) =
    clientWithRoute m (Proxy :: Proxy api) $
      req
        { requestHeaders =
            (hAuthorization, "Token " <> encodeUtf8 t) <| requestHeaders req
        }

wrappedParseJSON' :: FromJSON a => String -> Text -> Value -> Parser (Out a)
wrappedParseJSON' info key = wrappedParseJSON info key >=> \(In a) -> pure (Out a)

wrappedToJSON' :: ToJSON a => Text -> In a -> Value
wrappedToJSON' key (In a) = wrappedToJSON key $ Out a

instance ToHttpApiData a => ToHttpApiData (WithValidation a) where
  toUrlPiece (Failure err) = error $ "Invalid http api data: " <> show err
  toUrlPiece (Success a) = toUrlPiece a

deriving newtype instance ToHttpApiData Username

deriving newtype instance ToHttpApiData (IdOf "user")

deriving newtype instance ToHttpApiData Slug

deriving newtype instance ToHttpApiData (IdOf "article")

deriving newtype instance ToHttpApiData (IdOf "comment")

deriving newtype instance ToHttpApiData Tag

deriving newtype instance ToHttpApiData Limit

deriving newtype instance ToHttpApiData Offset

deriving newtype instance FromJSON Tag

deriving newtype instance FromJSON Slug

deriving newtype instance FromJSON (IdOf "user")

deriving newtype instance FromJSON (IdOf "article")

instance FromJSON (ContentOf "article")

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

deriving newtype instance FromJSON (IdOf "comment")

instance FromJSON (CommentR "withAuthorProfile")

instance FromJSON (Out (CommentR "withAuthorProfile")) where
  parseJSON = withObject "Out CommentR withAuthorProfile" $ \o -> Out <$> o .: "comment"

instance FromJSON (Out [CommentR "withAuthorProfile"]) where
  parseJSON = withObject "Out [ CommentR withAuthorProfile ]" $ \o -> Out <$> o .: "comments"

instance FromJSON (Out [Tag]) where
  parseJSON = withObject "Out [ Tag ]" $ \o -> Out <$> o .: "tags"

deriving newtype instance FromJSON (TokenOf "user")

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

instance ToJSON (LoginOf "user")

deriving newtype instance Eq a => Eq (In a)

-- instance ToJSON (In (WithValidation (LoginOf "user")))
instance ToJSON (In (LoginOf "user")) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (CreateOf "user")

instance ToJSON (In (CreateOf "user")) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (CreateOf "article")

instance ToJSON (In (CreateOf "article")) where
  toJSON = wrappedToJSON' "article"

instance ToJSON (CreateOf "comment")

instance ToJSON (In (CreateOf "comment")) where
  toJSON = wrappedToJSON' "comment"

instance ToJSON (Patch (UpdateOf "user")) where
  toJSON a =
    let insert' key = HM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "email") $ a ^. field' @"email"
        h2 = mayDo (insert' "username") $ a ^. field' @"username"
        h3 = mayDo (insert' "password") $ a ^. field' @"password"
        h4 = mayDo (insert' "bio") $ a ^. field' @"bio"
        h5 = mayDo (insert' "image") $ a ^. field' @"image"
     in Object $ h1 $ h2 $ h3 $ h4 $ h5 mempty

instance ToJSON (In (Patch (UpdateOf "user"))) where
  toJSON = wrappedToJSON' "user"

instance ToJSON (Patch (UpdateOf "article")) where
  toJSON a =
    let insert' key = HM.insert key . toJSON . SG.getLast
        mayDo = maybe Prelude.id
        h1 = mayDo (insert' "title") $ a ^. field' @"title"
        h2 = mayDo (insert' "description") $ a ^. field' @"description"
        h3 = mayDo (insert' "body") $ a ^. field' @"body"
     in Object $ h1 $ h2 $ h3 mempty

instance ToJSON (In (Patch (UpdateOf "article"))) where
  toJSON = wrappedToJSON' "article"

-- toEncoding  = wrappedToEncoding' "article"

-- | Password
--
-- @since 0.2.0.0
instance Eq Password where
  (==) = (==) `on` (\(Password pw) -> unsafeShowPassword pw)

deriving instance Show (UpdateOf "user")

deriving instance Show (UpdateOf "article")

deriving instance Eq (LoginOf "user")

deriving instance Eq (CreateOf "user")

deriving instance Eq (CreateOf "article")

deriving instance Eq (CreateOf "comment")

deriving instance Eq (UpdateOf "user")

-- for state machine, Set in model

deriving instance Ord Username

deriving instance Ord (IdOf "user")

deriving instance Ord Slug

deriving instance Ord (IdOf "article")

deriving instance Ord (IdOf "comment")

deriving instance Ord Tag

deriving newtype instance ToExpr Title

deriving newtype instance ToExpr Description

deriving newtype instance ToExpr Body

deriving newtype instance ToExpr Tag

instance ToExpr (CreateOf "article")

deriving newtype instance ToExpr Slug

deriving newtype instance ToExpr (IdOf "article")

instance ToExpr (CreateOf "comment")

deriving newtype instance ToExpr (IdOf "comment")

deriving newtype instance ToExpr Email

deriving newtype instance ToExpr Username

deriving newtype instance ToExpr Bio

deriving newtype instance ToExpr Image

instance ToExpr (AuthOf "user")

deriving newtype instance ToExpr (IdOf "user")

instance ToExpr (CreateOf "user")

instance ToExpr Password where
  toExpr = toExpr . show @Text

instance ToExpr (LoginOf "user")

deriving newtype instance ToExpr (TokenOf "user")
