{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Carrier with postgres storage
--
-- @since 0.4.0.0
module InRel8.Authentication.User where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.Authentication.HasAuth (AuthOf, LoginOf (UserLogin), NotAuthorized (BadPassword, NoSuchUser))
import Data.Domain (Domain (User))
import Data.Field.Email (Email)
import Data.Field.Password (checkPassword, hashPassword)
import Data.Password.Argon2 (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Data.Storage.Error (AlreadyExists (AlreadyExists))
import Data.Storage.Map (CreateOf (UserCreate), IdAlreadyExists, IdNotFound, IdOf (UserId))
import Data.Util.Impossible (Impossible (Impossible))
import Effect.Authentication (AuthenticationE (GetCurrentAuth, Login, Register))
import Effect.CreateSalt (CreateSaltE (CreateSalt))
import InRel8.Sql (SqlInRel8E (SqlSelect), insertOneRow)
import InRel8.Storage (getUserById, mkAuth)
import InRel8.Storage.Schema.User as UserRel8 (UserRel8 (UserRel8, email, password), userSchema)
import Rel8 (each, filter, lit, select, unsafeDefault, (==.))

-- | @since 0.4.0.0
newtype AuthenticationUserInRel8C m a = AuthenticationUserInRel8C
  { -- | @since 0.4.0.0
    runAuthenticationUserInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Algebra sig m,
    Member SqlInRel8E sig,
    Member CreateSaltE sig,
    Member (R.Reader (Maybe (AuthOf 'User))) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (Throw Impossible) sig,
    Member (Catch (IdNotFound 'User)) sig
  ) =>
  Algebra (AuthenticationE 'User :+: sig) (AuthenticationUserInRel8C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      let getUserByEmail em = send . SqlSelect . select $ each userSchema >>= Rel8.filter (\u -> UserRel8.email u ==. lit em)
      case action of
        Register (UserCreate (UserId -> uid) em pw) ->
          mkAuth <$> do
            salt <- send CreateSalt
            insertOneRow
              userSchema
              ( UserRel8
                  (lit uid)
                  (lit em)
                  (lit $ hashPassword pw salt)
                  unsafeDefault
                  unsafeDefault
                  unsafeDefault
                  unsafeDefault
              )
              Prelude.id
              $ do
                send (SqlSelect . select . getUserById $ lit uid) >>= \case
                  [] -> pure ()
                  [_] -> throwError $ AlreadyExists uid
                  _ -> throwError $ Impossible $ "get multiple user from id " <> show uid
                getUserByEmail em >>= \case
                  [] -> pure ()
                  [_] -> throwError $ AlreadyExists em
                  _ -> throwError $ Impossible $ "get multiple user from em" <> show em
                throwError $ Impossible "register insert failed"
        Login (UserLogin em pw) -> do
          getUserByEmail em >>= \case
            [] -> throwError $ NoSuchUser @'User
            [u] -> case checkPassword pw $ UserRel8.password u of
              PasswordCheckSuccess -> pure $ mkAuth u
              PasswordCheckFail -> throwError $ BadPassword @'User
            _ -> throwError $ Impossible "got mutiple users from single email"
        GetCurrentAuth -> R.ask
  alg hdl (R other) ctx = AuthenticationUserInRel8C $ alg (runAuthenticationUserInRel8 . hdl) other ctx
  {-# INLINE alg #-}
