{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- paging
--
-- @since 0.4.0.0
module Data.Paging where

import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import Servant (FromHttpApiData)

-- * Type

-- | @since 0.3.0.0
newtype Limit = Limit Natural deriving (Num, FromHttpApiData)

-- | @since 0.3.0.0
deriving via (WithNoValidation Natural) instance FromHttpApiData (WithValidation Limit)

-- | @since 0.3.0.0
newtype Offset = Offset Natural deriving (Num, FromHttpApiData)

-- | @since 0.3.0.0
deriving via (WithNoValidation Natural) instance FromHttpApiData (WithValidation Offset)

-- | @since 0.3.0.0
data Paging = LimitOffset Limit Offset

-- * Hepler

-- | @since 0.3.0.0
mkPaging :: Applicative f => Limit -> Offset -> Maybe (f Limit) -> Maybe (f Offset) -> f Paging
mkPaging dLimit dOffset mLimit mOffset =
  LimitOffset
    <$> (fromMaybe dLimit <$> sequenceA mLimit)
    <*> (fromMaybe dOffset <$> sequenceA mOffset)

-- * Typeclass

-- | @since 0.3.0.0
class HasPaging (f :: Type -> Type) where
  paging :: Paging -> f a -> f a

-- | @since 0.3.0.0
instance HasPaging [] where
  paging (LimitOffset (Limit lim) (Offset off)) = genericTake lim . genericDrop off
