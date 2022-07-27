{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Client.Internal.Orphans.ToHttpApiData where

import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Slug (Slug (Slug))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Username (Username (Username))
import Data.Paging (Limit (Limit), Offset (Offset))
import Data.Storage.Map.HasStorage (IdOf (..))
import Data.Util.Validation (WithValidation)
import Servant.API (ToHttpApiData (toUrlPiece))
import Validation (Validation (Failure, Success))

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
