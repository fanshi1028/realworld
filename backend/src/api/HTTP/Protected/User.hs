{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server for read or update current authed user's info
--
-- @since 0.1.0.0
module HTTP.Protected.User where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (User))
import HTTP.Util (ReadApi, UpdateApi)
import Domain.User (UserR)
import Servant (ServerT, type (:<|>) ((:<|>)))
import UserAction (E (GetCurrentUser, UpdateUser))
import Util.Error (ValidationErr)
import Util.JSON.From (In (In))
import Util.JSON.To (Out (Out))
import Validation (Validation (Failure, Success))

-- * API

-- | @since 0.1.0.0
type UserApi = ReadApi 'User (UserR "authWithToken") :<|> UpdateApi 'User (UserR "authWithToken")

-- * Server

-- | @since 0.2.0.0
userServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member UserAction.E sig
  ) =>
  ServerT UserApi m
userServer =
  Out <$> send GetCurrentUser
    :<|> ( \case
             In (Failure err) -> throwError err
             In (Success a) -> Out <$> send (UpdateUser a)
         )
