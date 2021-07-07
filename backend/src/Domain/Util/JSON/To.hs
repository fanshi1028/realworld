{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module Domain.Util.JSON.To where

import Data.Aeson (Encoding, GToJSON', ToJSON (toEncoding), Zero, defaultOptions, genericToEncoding)
import Data.Aeson.Encoding (int, text)
import Data.Aeson.Encoding.Internal (colon, comma, wrapObject, (><))
import GHC.Generics (Generic (Rep))

-- | wrapping type to make an "out" JSON representation
newtype Out a = Out a deriving (Show, Generic)

-- | wrap en encoding
wrapEncoding :: Text -> Encoding -> Encoding
wrapEncoding key encoding = wrapObject $ text key >< colon >< encoding

-- | toJSON wrapping helper function
wrappedToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => Text -> a -> Encoding
wrappedToEncoding key a = wrapEncoding key $ genericToEncoding defaultOptions a

-- | toJSON wrapping helper function (for mult row of data)
multiWrappedWithCountToEncoding :: (ToJSON (t a), Foldable t) => Text -> Text -> t a -> Encoding
multiWrappedWithCountToEncoding key countKey a =
  wrapObject $
    text key >< colon >< toEncoding a
      >< comma
      >< text countKey
      >< colon
      >< int (length a)
