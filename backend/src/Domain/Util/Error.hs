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

-- | TEMP: dangerous orphan instance
instance Exception ValidationErr

-- data Err a = SpecificErr a | AnyErr deriving (Show, Typeable, Generic, Functor, Exception)

newtype Err a = Err a

deriving newtype instance ToJSON a => ToJSON (Err a)

instance (Foldable t, ToJSON (t (Err a))) => ToJSON (Out (t (Err a))) where
  toEncoding = wrapEncoding "error" . wrappedToEncoding "body"

-- | Common Error
data NotFound a = NotFound a | SomethingNotFound deriving (Show, Generic, Exception)

instance (ToJSON a, Show a) => ToJSON (NotFound a) where
  toEncoding = Encoding . show

data AlreadyExists a = AlreadyExists a | SomethingAlreadyExists deriving (Show, Generic, Exception)

newtype NotAuthorized = NotAuthorized Text deriving (Show, Generic)

deriving anyclass instance Exception NotAuthorized

-- deriving anyclass instance (Typeable a, Show a) => Exception (NotAuthorized a)

type OtherErr = Text

type ValidationErr = NonEmpty Text

-- >>> show $ NotFound @Text "hi"
-- "NotFound \"hi\""

-- >>> import Data.UUID
-- >>> encode $ NotFound @UUID nil
-- "NotFound 00000000-0000-0000-0000-000000000000"

-- >>> encode $ SomethingNotFound @Text
-- "SomethingNotFound"
