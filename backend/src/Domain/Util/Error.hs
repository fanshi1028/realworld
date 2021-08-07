{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Domain.Util.Error where

import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding.Internal (Encoding' (Encoding))
import Domain.Util.JSON.To (Out, wrapEncoding, wrappedToEncoding)
import GHC.TypeLits (Symbol)

-- | TEMP: dangerous orphan instance
-- instance Exception ValidationErr

-- data Err a = SpecificErr a | AnyErr deriving (Show, Typeable, Generic, Functor, Exception)

newtype Err a = Err a

deriving newtype instance ToJSON a => ToJSON (Err a)

instance (Foldable t, ToJSON (t (Err a))) => ToJSON (Out (t (Err a))) where
  toEncoding = wrapEncoding "error" . wrappedToEncoding "body"

-- | Common Error
newtype NotFound a = NotFound a deriving (Show, Generic)

data SomethingNotFound = SomethingNotFound deriving (Show)

instance (ToJSON a, Show a) => ToJSON (NotFound a) where
  toEncoding = Encoding . show

newtype AlreadyExists a = AlreadyExists a deriving (Show)

data SomethingAlreadyExists = SomethingAlreadyExists deriving (Show)

data NotAuthorized (r :: Symbol -> Type) = NotAuthorized deriving (Show)

newtype Impossible = Impossible Text deriving (Show)

-- deriving anyclass instance Typeable r => Exception (NotAuthorized r)

type ValidationErr = NonEmpty Text

-- >>> show $ NotFound @Text "hi"
-- "NotFound \"hi\""

-- >>> import Data.UUID
-- >>> encode $ NotFound @UUID nil
-- "NotFound 00000000-0000-0000-0000-000000000000"

-- >>> encode $ SomethingNotFound @Text
-- "SomethingNotFound"
