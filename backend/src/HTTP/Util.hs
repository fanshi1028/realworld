{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module HTTP.Util (Limit (..), Offset (..), QP, CreateBody, UpdateBody, CreateApi, ReadApi, UpdateApi, DeleteApi, ReadManyApi, RUDApi, NoBodyUpdateApi, ToggleApi) where

import Domain.Util.Field (Tag, Username)
import Domain.Util.JSON.From (In)
import Domain.Util.JSON.To (Out)
import GHC.TypeLits (Symbol)
import Servant (Delete, FromHttpApiData, Get, JSON, Post, Put, QueryParam, ReqBody, type (:<|>), type (:>))
import Validation.Carrier.Selective (WithValidation)

-- Paging
newtype Limit = Limit Natural deriving (FromHttpApiData)

newtype Offset = Offset Natural deriving (FromHttpApiData)

-- QueryParam shorthand
type family QP' (s :: Symbol)

type instance QP' "tag" = Tag

type instance QP' "author" = Username

type instance QP' "favorited" = Username

type instance QP' "limit" = Limit

type instance QP' "offset" = Offset

type QP s = QueryParam s (QP' s)

-- ReqBody shorthand
type CreateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (WithValidation (r "create")))

type UpdateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (r "update"))

-- CRUD shorthand
type CreateApi (r :: Symbol -> Type) (o :: Symbol) = CreateBody r :> Post '[JSON] (Out (r o))

type ReadApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out (r o))

type ReadManyApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out [r o])

type UpdateApi (r :: Symbol -> Type) (o :: Symbol) = UpdateBody r :> Put '[JSON] (Out (r o))

type DeleteApi (r :: Symbol -> Type) (o :: Symbol) = Delete '[JSON] (Out (r o))

type NoBodyUpdateApi (r :: Symbol -> Type) (o :: Symbol) = Post '[JSON] (Out (r o))

-- common CRUD combination

type RUDApi (r :: Symbol -> Type) (o :: Symbol) = ReadApi r o :<|> UpdateApi r o :<|> DeleteApi r o

type ToggleApi (r :: Symbol -> Type) (o :: Symbol) = NoBodyUpdateApi r o :<|> DeleteApi r o
