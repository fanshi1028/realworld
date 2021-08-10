{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module Domain.Util.Error where

import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding.Internal (Encoding' (Encoding))
-- import Domain.Util.JSON.To (Out, wrapEncoding, wrappedToEncoding)
import GHC.TypeLits (Symbol)

-- newtype Err a = Err a

-- deriving newtype instance ToJSON a => ToJSON (Err a)

-- instance (Foldable t, ToJSON (t (Err a))) => ToJSON (Out (t (Err a))) where
--   toEncoding = wrapEncoding "error" . wrappedToEncoding "body"

-- | Common Error
newtype NotFound a = NotFound a deriving (Show, Generic)

instance (ToJSON a, Show a) => ToJSON (NotFound a) where
  toEncoding = Encoding . show

newtype AlreadyExists a = AlreadyExists a deriving (Show)

data NotAuthorized (r :: Symbol -> Type) = NotAuthorized deriving (Show)

data AlreadyLogin (r :: Symbol -> Type) = AlreadyLogin deriving (Show)

data NotLogin (r :: Symbol -> Type) = NotLogin deriving (Show)

newtype Impossible = Impossible Text deriving (Show)

type ValidationErr = NonEmpty Text
