{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server for read or update current authed user's info
--
-- @since 0.1.0.0
module HTTP.Protected.User where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.User (UserR (..))
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (ReadApi, UpdateApi)
import Servant (ServerT, type (:<|>) ((:<|>)))
import UserAction (E (GetCurrentUser, UpdateUser))

-- * API

-- | @since 0.1.0.0
type UserApi = ReadApi UserR "authWithToken" :<|> UpdateApi UserR "authWithToken"

-- * Server

-- | @since 0.1.0.0
userServer ::
  ( Algebra sig m,
    Member UserAction.E sig
  ) =>
  ServerT UserApi m
userServer = Out <$> send GetCurrentUser :<|> (\(In a) -> Out <$> send (UpdateUser a))
