{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Current.Reader (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Current (E (GetCurrent))
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized (NotAuthorized))
import Servant.Auth.Server (AuthResult (Authenticated))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (R.Reader (AuthResult (UserR "authWithToken"))) sig
  ) =>
  Algebra (E (UserR "authWithToken") :+: sig) (C m)
  where
  alg _ (L GetCurrent) ctx =
    (<$ ctx)
      <$> do
        R.ask >>= \case
          Authenticated u -> pure u
          _ -> throwError $ NotAuthorized @UserR
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
