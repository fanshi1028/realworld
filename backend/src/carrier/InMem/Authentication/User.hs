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
-- @since 0.3.0.0
module InMem.Authentication.User where

import Authentication.HasAuth (LoginOf (UserLogin), NotAuthorized (BadPassword, NoSuchUser), NotLogin)
import Authentication (AuthenticationE (Login, Register))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
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
import InMem.Relation (ToOne (getRelatedToOne, relateToOne), ToOneRelationE)
import InMem.Storage (MapInMemE, getByIdMapInMem, insertMapInMem)
import Storage.Error (AlreadyExists (AlreadyExists), NotFound)
import Storage.Map (ContentOf (..), CreateOf (UserCreate), IdAlreadyExists, IdNotFound, IdOf (UserId), toUserId)

-- | @since 0.3.0.0
newtype C gen m a = C
  { -- | @since 0.3.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( DRG gen,
    Algebra sig m,
    MapInMemE 'User sig,
    ToOneRelationE "EmailOfUser" sig,
    Member (Catch (IdNotFound 'User)) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (S.State gen) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig
  ) =>
  Algebra (AuthenticationE 'User :+: sig) (C gen m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      case action of
        Register (UserCreate user em pw) -> do
          let uid = UserId user
          -- FIXME: meta data like createdTime and UpdatedTime?
          -- send $ GetCurrent @Time
          u <-
            getRelatedToOne @"EmailOfUser" em >>= \case
              Just _ -> throwError $ AlreadyExists em
              Nothing -> do
                g <- S.get @gen
                let (salt, g') = withDRG g $ newSalt <$> getRandomBytes 16
                S.put g'
                catchError @(NotFound (IdOf 'User))
                  (getByIdMapInMem uid >> throwError (AlreadyExists uid))
                  $ const $ pure $ UserContent em (hashPassword pw salt) user (Bio "") $ Image ""
          insertMapInMem (toUserId u) u
          relateToOne @"EmailOfUser" em uid
          pure $ transform u
        Login (UserLogin em pw) ->
          getRelatedToOne @"EmailOfUser" em >>= \case
            Nothing -> throwError $ NoSuchUser @'User
            Just uid -> do
              a <- getByIdMapInMem uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ BadPassword @'User
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
