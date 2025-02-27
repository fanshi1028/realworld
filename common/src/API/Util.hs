{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Integrate HTTP server with validation and provide convenient alias
--
-- @since 0.4.0.0
module API.Util where

import Data.Domain (Domain)
import Data.Field.Tag (Tag)
import Data.Field.Username (Username)
import Data.Paging (Limit, Offset)
import Data.Storage.Map.HasCreate (CreateOf)
import Data.Storage.Map.HasUpdate (Patch, UpdateOf)
import Data.Util.JSON.From (In)
import Data.Util.JSON.To (Out)
import Data.Util.Validation (WithValidation)
import GHC.TypeLits (Symbol)
import Servant (Capture, Delete, Get, JSON, NoContent, NoFraming, Post, Put, QueryParam, ReqBody, SourceIO, StreamGet, type (:<|>), type (:>))

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

-- | @since 0.3.0.0
type ReadApi o = Get '[JSON] (Out o)

-- | @since 0.3.0.0
type ReadManyApi o = ReadApi [o] :<|> "stream" :> StreamGet NoFraming JSON (SourceIO o)

-- ** Update

-- | @since 0.1.0.0
type UpdateApi (s :: Domain) o = UpdateBody s :> Put '[JSON] (Out o)

-- | @since 0.1.0.0
type NoBodyUpdateApi o = Post '[JSON] (Out o)

-- ** Combinations

-- | Update with Delete
--
-- @since 0.1.0.0
type UDApi (s :: Domain) o = UpdateApi s o :<|> Delete '[JSON] NoContent

-- | For togglable state
--
-- @since 0.1.0.0
type ToggleApi o = NoBodyUpdateApi o :<|> Delete '[JSON] (Out o)
