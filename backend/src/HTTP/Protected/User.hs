{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Protected.User (UserApi, userServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.User (UserR (..))
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (ReadApi, UpdateApi)
import Servant (ServerT, type (:<|>) ((:<|>)))
import UserAction (E (GetCurrentUser, UpdateUser))

type UserApi = ReadApi UserR "authWithToken" :<|> UpdateApi UserR "authWithToken"

userServer ::
  ( Algebra sig m,
    Member UserAction.E sig
  ) =>
  ServerT UserApi m
userServer = Out <$> send GetCurrentUser :<|> (\(In a) -> Out <$> send (UpdateUser a))
