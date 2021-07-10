{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.User (UserApi, userServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Sum (Member)
import Domain.User (UserR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (CreateApi)
import Relude.Extra (un)
import Servant (JSON, Post, ReqBody, ServerT, type (:<|>) ((:<|>)), type (:>))
import Validation (validation)
import Validation.Carrier.Selective (WithValidation)
import VisitorAction (E (Login, Register))

type UserApi =
  "login" :> ReqBody '[JSON] (In (WithValidation (UserR "login"))) :> Post '[JSON] (Out (UserR "auth"))
    :<|> CreateApi UserR "auth"

userServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member VisitorAction.E sig
  ) =>
  ServerT UserApi m
userServer =
  let validateThen action = validation (throwError @ValidationErr) (Out <<$>> send . action) . un
   in validateThen Login :<|> validateThen Register
