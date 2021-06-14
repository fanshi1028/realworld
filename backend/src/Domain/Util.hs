{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.Util where

import Data.Aeson (Encoding, FromJSON (parseJSON), GToJSON', Object, ToJSON (toEncoding), Value, Zero, defaultOptions, genericToEncoding, withObject, withText, (.:), (<?>))
import Data.Aeson.Encoding (int, text)
import Data.Aeson.Encoding.Internal (colon, comma, wrapObject, (><))
import Data.Aeson.Types (JSONPathElement (Key), Parser)
import qualified Data.Text as T (null)
import Data.Time (UTCTime)
import GHC.Generics (Generic (Rep))
import Relude.Extra (insertWith)
import Text.Show (Show (showsPrec), showString)
import Validation.Adaptor (NoValidation, NoValidation' (..), WithValidation, validate)

-- | helper to override and provide default value when writing FromJSON instance
insert' :: Text -> Value -> Object -> Object
insert' = insertWith @Object (\_ x -> x)

-- | some newtype

-- | Email
newtype Email = Email Text deriving newtype (Show, Eq, ToJSON)

instance FromJSON (WithValidation Email) where
  parseJSON = withText "email" $ pure <$> (Email <<$>> validate (not . T.null) "null email")

-- | Password
newtype Password = Password Text deriving newtype (Eq, ToJSON)

instance FromJSON (WithValidation Password) where
  parseJSON = withText "pw" $ pure <$> (Password <<$>> validate (not . T.null) "null password")

instance Show Password where
  showsPrec _ _ = showString "********"

-- | Username
newtype Username = Username Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Username)

-- | Token
newtype Token = Token Text deriving newtype (Show, Eq, ToJSON)

-- | Bio
newtype Bio = Bio Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Bio)

-- | Image
newtype Image = Image Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

deriving via NoValidation instance FromJSON (WithValidation Image)

newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON)

newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON)

-- deriving (Generic)

-- deriving via NoValidation instance FromJSON (WithValidation Title)

instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure <$> (Title <<$>> validate (not . T.null) "null title")

newtype Description = Description Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Description)

newtype Body = Body Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Body)

newtype Tag = Tag Text deriving newtype (Show, Eq, ToJSON)

deriving via NoValidation instance FromJSON (WithValidation Tag)

instance (Foldable t, ToJSON (t Tag)) => ToJSON (Out (t Tag)) where
  toEncoding = wrappedToEncoding "tags"

data Error a = SpecificErr a | AnyErr deriving (Generic)

instance ToJSON a => ToJSON (Error a)

instance (Foldable t, ToJSON (t (Error a))) => ToJSON (Out (t (Error a))) where
  toEncoding a = wrapObject $ text "error" >< colon >< wrappedToEncoding "body" a

type Time = UTCTime

-- | wrapping type to make an "out" JSON representation
newtype Out a = Out a deriving (Show, Generic)

-- | toJSON wrapping helper function
wrappedToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => Text -> a -> Encoding
wrappedToEncoding key a = wrapObject $ text key >< colon >< genericToEncoding defaultOptions a

-- | toJSON wrapping helper function (for mult row of data)
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Text -> Text -> t a -> Encoding
multiWrappedWithCountToEncoding key countKey a =
  wrapObject $
    text key >< colon >< toEncoding a
      >< comma
      >< text countKey
      >< colon
      >< int (length a)

-- | wrapping type to make an "in" JSON representation
newtype In a = In a deriving (Show, Generic)

wrappedParseJSON :: FromJSON a => String -> Text -> Value -> Parser (In a)
wrappedParseJSON info key = withObject info $ \o -> In <$> (o .: key >>= (<?> Key key) . parseJSON)
