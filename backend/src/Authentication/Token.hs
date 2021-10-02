{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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

import qualified Authentication (E (Login, Logout, Register))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Password.Argon2 (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), checkPassword, hashPassword)
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists (AlreadyExists), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Util.Field (Bio (Bio), Email, Image (Image), Time)
import Domain.Util.Representation (transform)
import GHC.Records (getField)
import GHC.TypeLits (Symbol)
import qualified Relation.ToOne
import qualified Storage.Map
import qualified Token (E (InvalidateToken))

-- | @since 0.1.0.0
newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Lift IO) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Relation.ToOne.E Email "of" (UserR "id")) sig,
    Member (Catch (NotFound (UserR "id"))) sig,
    Member (Throw (AlreadyExists (UserR "id"))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Current.E Time) sig,
    Member (Throw (NotFound Email)) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Token.E UserR) sig
  ) =>
  Algebra (Authentication.E UserR :+: sig) (C UserR m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      case action of
        Authentication.Register (UserRegister user em pw) -> do
          -- FIXME: meta data like createdTime and UpdatedTime?
          -- send $ GetCurrent @Time
          u <-
            send (Relation.ToOne.GetRelated @_ @"of" @(UserR "id") em) >>= \case
              Just _ -> throwError $ AlreadyExists em
              Nothing ->
                catchError @(NotFound (UserR "id"))
                  (send (Storage.Map.GetById $ UserId user) >> throwError (AlreadyExists $ UserId user))
                  $ const $ sendIO (hashPassword pw) <&> \hash -> User em hash user (Bio "") (Image "")
          send $ Storage.Map.Insert @UserR u
          send $ Relation.ToOne.Relate @_ @(UserR "id") @"of" em $ UserId user
          pure $ transform u
        Authentication.Logout -> do
          UserAuthWithToken _ t <- send $ Current.GetCurrent @(UserR "authWithToken")
          send $ Token.InvalidateToken t
        Authentication.Login (UserLogin em pw) ->
          send (Relation.ToOne.GetRelated @_ @"of" em) >>= \case
            Nothing -> throwError $ NotFound em
            Just uid -> do
              a <- send $ Storage.Map.GetById @UserR uid
              case checkPassword pw $ getField @"password" a of
                PasswordCheckSuccess -> pure $ transform a
                PasswordCheckFail -> throwError $ NotAuthorized @UserR
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
