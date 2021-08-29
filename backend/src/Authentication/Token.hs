{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) fanshi1028 , 2021
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
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Util.Field (Email, Time)
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
        Authentication.Register user -> do
          a <- transform user
          send $ Storage.Map.Insert @UserR a
          transform user >>= send . Relation.ToOne.Relate @_ @(UserR "id") @"of" (getField @"email" user)
          transform a
        Authentication.Logout ->
          send (Current.GetCurrent @(UserR "authWithToken"))
            >>= \(UserAuthWithToken _ t) -> send $ Token.InvalidateToken t
        Authentication.Login (UserLogin em pw) ->
          send (Relation.ToOne.GetRelated @_ @"of" em) >>= \case
            Nothing -> throwError $ NotFound em
            Just uid ->
              send (Storage.Map.GetById @UserR uid) >>= \a ->
                -- FIXME: pw stuff
                if getField @"password" a == pw
                  then transform a
                  else throwError $ NotAuthorized @UserR
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
