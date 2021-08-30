{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Integrate HTTP server with validation and provide convenient alias
--
-- @since 0.1.0.0
module HTTP.Util where

import Domain.Util.Field (Tag, Username)
import Domain.Util.JSON.From (In)
import Domain.Util.JSON.To (Out)
import Domain.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import GHC.TypeLits (Symbol)
import Servant (Capture, Delete, FromHttpApiData, Get, JSON, NoContent, Post, Put, QueryParam, ReqBody, type (:<|>), type (:>))

-- * Paging

-- | @since 0.1.0.0
newtype Limit = Limit Natural deriving (FromHttpApiData)

-- | @since 0.1.0.0
deriving via (WithNoValidation Natural) instance FromHttpApiData (WithValidation Limit)

-- | @since 0.1.0.0
newtype Offset = Offset Natural deriving (FromHttpApiData)

-- | @since 0.1.0.0
deriving via (WithNoValidation Natural) instance FromHttpApiData (WithValidation Offset)

-- * QueryParam

-- | Type family to map query parameters' key to their capturing type.
--
-- @since 0.1.0.0
type family QP' (s :: Symbol)

-- | @since 0.1.0.0
type instance QP' "tag" = Tag

-- | @since 0.1.0.0
type instance QP' "author" = Username

-- | @since 0.1.0.0
type instance QP' "favorited" = Username

-- | @since 0.1.0.0
type instance QP' "limit" = Limit

-- | @since 0.1.0.0
type instance QP' "offset" = Offset

-- | Convenient alias.
-- Integrate query params with validation.
-- You will probably use this instead of the unvalidated 'QP''
--
-- @since 0.1.0.0
type QP s = QueryParam s (WithValidation (QP' s))

-- * Capture

-- | Convenient alias.
-- Integrate captures in routes with validation.
--
-- @since 0.1.0.0
type Cap s r = Capture s (WithValidation r)

-- * ReqBody

-- | Convenient alias.
--
-- @since 0.1.0.0
type CreateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (WithValidation (r "create")))

-- | Convenient alias.
--
-- @since 0.1.0.0
type UpdateBody (r :: Symbol -> Type) = ReqBody '[JSON] (In (r "update"))

-- * CRUD

-- ** Create

-- | @since 0.1.0.0
type CreateApi (r :: Symbol -> Type) (o :: Symbol) = CreateBody r :> Post '[JSON] (Out (r o))

-- ** Read

-- | @since 0.1.0.0
type ReadApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out (r o))

-- | @since 0.1.0.0
type ReadManyApi (r :: Symbol -> Type) (o :: Symbol) = Get '[JSON] (Out [r o])

-- ** Update

-- | @since 0.1.0.0
type UpdateApi (r :: Symbol -> Type) (o :: Symbol) = UpdateBody r :> Put '[JSON] (Out (r o))

-- | @since 0.1.0.0
type NoBodyUpdateApi (r :: Symbol -> Type) (o :: Symbol) = Post '[JSON] (Out (r o))

-- ** Combinations

-- | Update with Delete
--
-- @since 0.1.0.0
type UDApi (r :: Symbol -> Type) (o :: Symbol) = UpdateApi r o :<|> Delete '[JSON] NoContent

-- | For togglable state
--
-- @since 0.1.0.0
type ToggleApi (r :: Symbol -> Type) (o :: Symbol) = NoBodyUpdateApi r o :<|> Delete '[JSON] (Out (r o))
