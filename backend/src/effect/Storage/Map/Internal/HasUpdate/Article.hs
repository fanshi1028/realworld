{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasUpdate.Article where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import Data.Generic.HKD (HKD, build, construct)
import Data.Generics.Product (HasField' (field'))
import Data.Generics.Product.Fields (getField)
import qualified Data.Semigroup as SG
import Domain (Domain (Article))
import Field.Body (Body)
import Field.Description (Description)
import Field.Title (Title)
import Relude.Extra ((.~))
import Storage.Map.Internal.HasStorage (ContentOf)
import Storage.Map.Internal.HasStorage.Article ()
import Storage.Map.Internal.HasUpdate (HasUpdate (UpdateOf), Patch, updatableKeys)
import Util.JSON.From (In, wrappedParseJSON)
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance HasUpdate 'Article where
  data UpdateOf 'Article = ArticleUpdate
    { title :: Title,
      description :: Description,
      body :: Body
    }
    deriving (Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (Patch (UpdateOf 'Article))) where
  parseJSON = withObject "UpdateArticle" $ \o -> do
    updatableKeys ["title", "description", "body"] o
    construct
      <$> construct
        ( build @(HKD (HKD (HKD (UpdateOf 'Article) SG.Last) Maybe) WithValidation)
            (o .:? "title" .!= pure Nothing)
            (o .:? "description" .!= pure Nothing)
            (o .:? "body" .!= pure Nothing)
        )

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (Patch (UpdateOf 'Article)))) where
  parseJSON = wrappedParseJSON "ArticleUpdate" "article"

toArticlePatch :: Patch (UpdateOf 'Article) -> Patch (ContentOf 'Article)
toArticlePatch update =
  mempty @(Patch (ContentOf 'Article))
    & field' @"title" .~ getField @"title" update
    & field' @"description" .~ getField @"description" update
    & field' @"body" .~ getField @"body" update
