{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some IsAuth instances for servant
--
-- @since 0.1.0.0
module InMem.Authorization
  ( -- * TokenAuthInMem
    TokenAuthInMem,
  )
where

import API.Authorization (pattern RequestToken)
import Data.Authentication.HasToken (TokenOf (..))
import Data.Domain (Domain (User))
import Data.Domain.Transform (Transform (transform))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Data.Storage.Map.HasStorage (IdOf)
import InMem.Storage (TableInMem)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM (lookup)
import qualified StmContainers.Map as STMMap (Map)

-- | @since 0.1.0.0
-- Use hand-roll in-memory storage to facilitate auth process
data TokenAuthInMem

-- | @since 0.4.0.0
instance IsAuth TokenAuthInMem UserAuthWithToken where
  type AuthArgs TokenAuthInMem = '[TableInMem 'User, STMMap.Map (TokenOf 'User) (IdOf 'User)]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    RequestToken token ->
      atomically $
        STM.lookup token tokenDb
          >>= traverse (`STM.lookup` userDb)
          <&> \case
            (join -> Just u) -> pure $ UserAuthWithToken (transform u) token
            _ -> mempty
    _ -> pure mempty
