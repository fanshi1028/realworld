{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier using token
--
-- @since 0.1.0.0
module Authentication.Token where

import Authentication (LoginOf (UserLogin))
import qualified Authentication (E (Login, Logout, Register))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Password.Argon2 (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Field.Bio (Bio (Bio))
import Field.Email (Email)
import Field.Image (Image (Image))
import Field.Password (checkPassword, hashPassword, newSalt)
import Field.Time (Time)
import GHC.Records (getField)
import GHC.TypeLits (Symbol)
import qualified Relation.ToOne
import Storage.Map (ContentOf (User), CreateOf (UserCreate), IdOf (UserId), toUserId)
import qualified Storage.Map
import qualified Token (E (InvalidateToken))
import User (UserR (UserAuthWithToken))
import Util.Error (AlreadyExists (AlreadyExists), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Util.Representation (transform)

-- | @since 0.1.0.0
newtype C (s :: Symbol) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Lift IO) sig,
    Member (Throw (NotAuthorized (IdOf "user"))) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Relation.ToOne.E Email "of" (IdOf "user")) sig,
    Member (Catch (NotFound (IdOf "user"))) sig,
    Member (Throw (AlreadyExists (IdOf "user"))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Current.E Time) sig,
    Member (Throw (NotFound Email)) sig,
    Member (Storage.Map.E "user") sig,
    Member (Token.E "user") sig
  ) =>
  Algebra (Authentication.E "user" :+: sig) (C "user" m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      case action of
        Authentication.Register (UserCreate user em pw) -> do
          let uid = UserId user
          -- FIXME: meta data like createdTime and UpdatedTime?
          -- send $ GetCurrent @Time
          u <-
            send (Relation.ToOne.GetRelated @_ @"of" @(IdOf "user") em) >>= \case
              Just _ -> throwError $ AlreadyExists em
              Nothing ->
                catchError @(NotFound (IdOf "user"))
                  (send (Storage.Map.GetById uid) >> throwError (AlreadyExists uid))
                  $ const $ sendIO newSalt <&> \(hashPassword pw -> hash) -> User em hash user (Bio "") (Image "")
          send $ Storage.Map.Insert (toUserId u) u
          send $ Relation.ToOne.Relate @_ @_ @"of" em uid
          pure $ transform u
        Authentication.Logout -> do
          UserAuthWithToken _ t <- send $ Current.GetCurrent @(UserR "authWithToken")
          send $ Token.InvalidateToken t
        Authentication.Login (UserLogin em pw) ->
          send (Relation.ToOne.GetRelated @_ @"of" @(IdOf "user") em) >>= \case
            Nothing -> throwError $ NotFound em
            Just uid -> do
              a <- send $ Storage.Map.GetById uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ NotAuthorized uid
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
