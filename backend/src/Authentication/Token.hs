{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Token (E (..)) where

import qualified Authentication (E (Login, Logout))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Domain.User (UserR (..))
import Domain.Util.Error (NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Util.Field (Email)
import GHC.Records (getField)
import GHC.TypeLits (Symbol)
import qualified Relation.ToOne
import qualified Storage.Map

data E (r :: Symbol -> Type) (m :: Type -> Type) a where
  CheckToken :: r "token" -> E r m (r "auth")
  CreateToken :: r "auth" -> E r m (r "token")
  InvalidateToken :: r "token" -> E r m ()

newtype C (r :: Symbol -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Relation.ToOne.E Email "of" (UserR "id")) sig,
    Member (Throw (NotFound Email)) sig,
    Member (Storage.Map.E UserR) sig,
    Member (E UserR) sig
  ) =>
  Algebra (Authentication.E UserR :+: sig) (C UserR m)
  where
  alg _ (L Authentication.Logout) ctx =
    send (Current.GetCurrent @(UserR "authWithToken")) >>= \case
      UserAuthWithToken _ t -> (<$ ctx) <$> send (InvalidateToken t)
  alg _ (L (Authentication.Login (UserLogin em pw))) ctx =
    send (Relation.ToOne.GetRelated @_ @"of" em) >>= \case
      Nothing -> throwError $ NotFound em
      Just uid ->
        send (Storage.Map.GetById @UserR uid)
          >>= \a ->
            -- FIXME: pw stuff
            if getField @"password" a == pw
              then pure $ uid <$ ctx
              else throwError $ NotAuthorized @UserR
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
