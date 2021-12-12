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

import Authentication (AuthenticationE (GetCurrentAuth, Login, Register))
import Authentication.HasAuth (AuthOf, LoginOf (UserLogin), NotAuthorized (BadPassword, NoSuchUser), NotLogin (NotLogin))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import CreateSalt (CreateSaltE (CreateSalt))
import Data.Password.Argon2 (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Domain (Domain (User))
import Domain.Transform (transform)
import Field.Bio (Bio (Bio))
import Field.Email (Email)
import Field.Image (Image (Image))
import Field.Password (checkPassword, hashPassword)
import GHC.Records (getField)
import InMem.Relation (EmailOfUser, ToOne (getRelatedToOne, relateToOne), ToOneRelationE)
import InMem.Storage (MapInMemE, getByIdMapInMem, insertMapInMem)
import Storage.Error (AlreadyExists (AlreadyExists), NotFound)
import Storage.Map (ContentOf (..), CreateOf (UserCreate), IdAlreadyExists, IdNotFound, IdOf (UserId), toUserId)

-- | @since 0.3.0.0
newtype AuthenticationUserInMemC m a = AuthenticationUserInMemC
  { -- | @since 0.3.0.0
    runAuthenticationUserInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( Algebra sig m,
    MapInMemE 'User sig,
    ToOneRelationE EmailOfUser sig,
    Member (Catch (IdNotFound 'User)) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member CreateSaltE sig,
    Member (R.Reader (Maybe (AuthOf 'User))) sig
  ) =>
  Algebra (AuthenticationE 'User :+: sig) (AuthenticationUserInMemC m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      case action of
        Register (UserCreate user em pw) -> do
          let uid = UserId user
          -- FIXME: meta data like createdTime and UpdatedTime?
          -- send $ GetCurrent @Time
          u <-
            getRelatedToOne @EmailOfUser em >>= \case
              Just _ -> throwError $ AlreadyExists em
              Nothing -> do
                salt <- send CreateSalt
                catchError @(NotFound (IdOf 'User))
                  (getByIdMapInMem uid >> throwError (AlreadyExists uid))
                  $ const $ pure $ UserContent em (hashPassword pw salt) user (Bio "") $ Image ""
          insertMapInMem (toUserId u) u
          relateToOne @EmailOfUser em uid
          pure $ transform u
        Login (UserLogin em pw) ->
          getRelatedToOne @EmailOfUser em >>= \case
            Nothing -> throwError $ NoSuchUser @'User
            Just uid -> do
              a <- getByIdMapInMem uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ BadPassword @'User
        GetCurrentAuth -> R.ask
  alg hdl (R other) ctx = AuthenticationUserInMemC $ alg (runAuthenticationUserInMem . hdl) other ctx
  {-# INLINE alg #-}
