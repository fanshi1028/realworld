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

import Domain (Domain)
import Field.Tag (Tag)
import Field.Username (Username)
import GHC.TypeLits (Symbol)
import Servant (Capture, Delete, FromHttpApiData, Get, JSON, NoContent, Post, Put, QueryParam, ReqBody, type (:<|>), type (:>))
import Storage.Map (CreateOf, Patch, UpdateOf)
import Util.JSON.From (In)
import Util.JSON.To (Out)
import Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

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
type CreateBody (s :: Domain) = ReqBody '[JSON] (In (WithValidation (CreateOf s)))

-- | Convenient alias.
--
-- @since 0.2.0.0
type UpdateBody (s :: Domain) = ReqBody '[JSON] (In (WithValidation (Patch (UpdateOf s))))

-- * CRUD

-- ** Create

-- | @since 0.1.0.0
type CreateApi (s :: Domain) o = CreateBody s :> Post '[JSON] (Out o)

-- ** Read

-- | @since 0.1.0.0
type ReadApi (s :: Domain) o = Get '[JSON] (Out o)

-- | @since 0.1.0.0
type ReadManyApi (s :: Domain) o = Get '[JSON] (Out [o])

-- ** Update

-- | @since 0.1.0.0
type UpdateApi (s :: Domain) o = UpdateBody s :> Put '[JSON] (Out o)

-- | @since 0.1.0.0
type NoBodyUpdateApi (s :: Domain) o = Post '[JSON] (Out o)

-- ** Combinations

-- | Update with Delete
--
-- @since 0.1.0.0
type UDApi (s :: Domain) o = UpdateApi s o :<|> Delete '[JSON] NoContent

-- | For togglable state
--
-- @since 0.1.0.0
type ToggleApi (s :: Domain) o = NoBodyUpdateApi s o :<|> Delete '[JSON] (Out o)
