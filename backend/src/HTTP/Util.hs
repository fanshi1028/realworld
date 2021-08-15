{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module HTTP.Util
  ( Limit (..),
    Offset (..),
    QP,
    CreateBody,
    UpdateBody,
    CreateApi,
    ReadApi,
    UpdateApi,
    ReadManyApi,
    NoBodyUpdateApi,
    ToggleApi,
    UDApi,
  )
where

import Domain.Util.Field (Tag, Username)
import Domain.Util.JSON.From (In)
import Domain.Util.JSON.To (Out)
import Domain.Util.Validation (WithValidation)
import GHC.TypeLits (Symbol)
import Servant (Delete, FromHttpApiData (parseQueryParam), Get, JSON, NoContent, Post, Put, QueryParam, ReqBody, type (:<|>), type (:>))

-- Paging
newtype Limit = Limit Natural deriving (FromHttpApiData)

instance FromHttpApiData (WithValidation Limit) where
  parseQueryParam = pure <<$>> parseQueryParam

newtype Offset = Offset Natural deriving (FromHttpApiData)

instance FromHttpApiData (WithValidation Offset) where
  parseQueryParam = pure <<$>> parseQueryParam

-- QueryParam shorthand
type family QP' (s :: Symbol)

type instance QP' "tag" = Tag

type instance QP' "author" = Username

type instance QP' "favorited" = Username

type instance QP' "limit" = Limit

type instance QP' "offset" = Offset

type QP s = QueryParam s (WithValidation (QP' s))

-- ReqBody shorthand
type CreateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (WithValidation (r "create")))

type UpdateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (r "update"))

-- CRUD shorthand
type CreateApi (r :: Symbol -> Type) (o :: Symbol) = CreateBody r :> Post '[JSON] (Out (r o))

type ReadApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out (r o))

type ReadManyApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out [r o])

type UpdateApi (r :: Symbol -> Type) (o :: Symbol) = UpdateBody r :> Put '[JSON] (Out (r o))

type NoBodyUpdateApi (r :: Symbol -> Type) (o :: Symbol) = Post '[JSON] (Out (r o))

-- common CRUD combination

type UDApi (r :: Symbol -> Type) (o :: Symbol) = UpdateApi r o :<|> Delete '[JSON] NoContent

type ToggleApi (r :: Symbol -> Type) (o :: Symbol) = NoBodyUpdateApi r o :<|> Delete '[JSON] (Out (r o))
