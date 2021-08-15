{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
  Cap)
where

import Domain.Util.Field (Tag, Username)
import Domain.Util.JSON.From (In)
import Domain.Util.JSON.To (Out)
import Domain.Util.Validation (NoValidation, NoValidation' (..), WithValidation)
import GHC.TypeLits (Symbol)
import Servant (Delete, FromHttpApiData, Get, JSON, NoContent, Post, Put, QueryParam, ReqBody, type (:<|>), type (:>), Capture)

-- Paging
newtype Limit = Limit Natural deriving (FromHttpApiData)

deriving via (NoValidation Natural) instance FromHttpApiData (WithValidation Limit)

newtype Offset = Offset Natural deriving (FromHttpApiData)

deriving via (NoValidation Natural) instance FromHttpApiData (WithValidation Offset)

-- QueryParam shorthand
type family QP' (s :: Symbol)

type instance QP' "tag" = Tag

type instance QP' "author" = Username

type instance QP' "favorited" = Username

type instance QP' "limit" = Limit

type instance QP' "offset" = Offset

type QP s = QueryParam s (WithValidation (QP' s))

-- Capture shorthand
type Cap s r = Capture s (WithValidation r)

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
