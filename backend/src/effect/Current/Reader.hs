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
-- Carrier in Reader
--
-- @since 0.1.0.0
module Current.Reader where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Current (E (GetCurrent))
import Domain.User (UserR)
import Servant.Auth.Server (AuthResult (Authenticated))
import Util.Error (NotAuthorized (NotAuthorized))

-- | @since 0.1.0.0
newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | Only instance for 'AuthResult' ('UserR' \"authWithToken\") for now
--
-- @since 0.2.0.0
instance
  ( Algebra sig m,
    Member (Throw (NotAuthorized ())) sig,
    Member (R.Reader (AuthResult (UserR "authWithToken"))) sig
  ) =>
  Algebra (E (UserR "authWithToken") :+: sig) (C m)
  where
  alg _ (L GetCurrent) ctx =
    R.ask >>= \case
      Authenticated u -> pure $ u <$ ctx
      _ -> throwError $ NotAuthorized ()
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
