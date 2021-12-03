{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Authentication.User where

import Authentication (E, LoginOf (UserLogin), NotAuthorized (BadPassword, NoSuchUser), NotLogin)
import qualified Authentication (E (Login, Register))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import qualified Control.Effect.Reader as R (Reader)
import qualified Control.Effect.State as S (State, get, put)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.Random (DRG, getRandomBytes, withDRG)
import Data.Password.Argon2 (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Domain (Domain (User))
import Domain.Transform (transform)
import Domain.User (UserR)
import Field.Bio (Bio (Bio))
import Field.Email (Email)
import Field.Image (Image (Image))
import Field.Password (checkPassword, hashPassword, newSalt)
import GHC.Records (getField)
import qualified Relation.ToOne (E (GetRelated, Relate))
import Storage.Error (AlreadyExists (AlreadyExists), NotFound)
import Storage.InMem (MapInMemE, getByIdMapInMem, insertMapInMem)
import Storage.Map (ContentOf (..), CreateOf (UserCreate), IdAlreadyExists, IdNotFound, IdOf (UserId), toUserId)

-- | @since 0.1.0.0
newtype C gen m a = C
  { -- | @since 0.1.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( DRG gen,
    Algebra sig m,
    MapInMemE 'User sig,
    Member (Relation.ToOne.E Email "of" (IdOf 'User)) sig,
    Member (Catch (IdNotFound 'User)) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (S.State gen) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig
  ) =>
  Algebra (Authentication.E 'User :+: sig) (C gen m)
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
              Nothing -> do
                g <- S.get @gen
                let (salt, g') = withDRG g $ newSalt <$> getRandomBytes 16
                S.put g'
                catchError @(NotFound (IdOf 'User))
                  (getByIdMapInMem uid >> throwError (AlreadyExists uid))
                  $ const $ pure $ UserContent em (hashPassword pw salt) user (Bio "") $ Image ""
          insertMapInMem (toUserId u) u
          send $ Relation.ToOne.Relate @_ @_ @"of" em uid
          pure $ transform u
        Authentication.Login (UserLogin em pw) ->
          send (Relation.ToOne.GetRelated @_ @"of" @(IdOf 'User) em) >>= \case
            Nothing -> throwError $ NoSuchUser @'User
            Just uid -> do
              a <- getByIdMapInMem uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ BadPassword @'User
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
