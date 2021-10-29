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
import Domain (Domain (User))
import Field.Bio (Bio (Bio))
import Field.Email (Email)
import Field.Image (Image (Image))
import Field.Password (checkPassword, hashPassword, newSalt)
import Field.Time (Time)
import GHC.Records (getField)
import qualified Relation.ToOne
import Domain.User (UserR (UserAuthWithToken))
import Storage.Map (ContentOf (UserContent), CreateOf (UserCreate), IdOf (UserId), toUserId)
import qualified Storage.Map
import qualified Token (E (InvalidateToken))
import Util.Error (AlreadyExists (AlreadyExists), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Transform (transform)

-- | @since 0.1.0.0
newtype C (s :: Domain) m a = C
  { -- | @since 0.1.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Lift IO) sig,
    Member (Throw (NotAuthorized (IdOf 'User))) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Relation.ToOne.E Email "of" (IdOf 'User)) sig,
    Member (Catch (NotFound (IdOf 'User))) sig,
    Member (Throw (AlreadyExists (IdOf 'User))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Current.E Time) sig,
    Member (Throw (NotFound Email)) sig,
    Member (Storage.Map.E 'User) sig,
    Member (Token.E 'User) sig
  ) =>
  Algebra (Authentication.E 'User :+: sig) (C 'User m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      case action of
        Authentication.Register (UserCreate user em pw) -> do
          let uid = UserId user
          -- FIXME: meta data like createdTime and UpdatedTime?
          -- send $ GetCurrent @Time
          u <-
            send (Relation.ToOne.GetRelated @_ @"of" @(IdOf 'User) em) >>= \case
              Just _ -> throwError $ AlreadyExists em
              Nothing ->
                catchError @(NotFound (IdOf 'User))
                  (send (Storage.Map.GetById uid) >> throwError (AlreadyExists uid))
                  $ const $ sendIO newSalt <&> \(hashPassword pw -> hash) -> UserContent em hash user (Bio "") (Image "")
          send $ Storage.Map.Insert (toUserId u) u
          send $ Relation.ToOne.Relate @_ @_ @"of" em uid
          pure $ transform u
        Authentication.Logout -> do
          UserAuthWithToken _ t <- send $ Current.GetCurrent @(UserR "authWithToken")
          send $ Token.InvalidateToken t
        Authentication.Login (UserLogin em pw) ->
          send (Relation.ToOne.GetRelated @_ @"of" @(IdOf 'User) em) >>= \case
            Nothing -> throwError $ NotFound em
            Just uid -> do
              a <- send $ Storage.Map.GetById uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ NotAuthorized uid
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
