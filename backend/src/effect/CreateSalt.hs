{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of create salt
--
-- @since 0.1.0.0
module CreateSalt (CreateSaltE (CreateSalt), CreateSaltC (runCreateSalt)) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Control.Effect.State as S (State, get, put)
import Control.Effect.Sum (Member)
import Crypto.Random (DRG, MonadRandom (getRandomBytes), withDRG)
import Field.Password (Salt, newSalt)

-- * Effect

-- | @since 0.3.0.0
data CreateSaltE (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  CreateSalt :: CreateSaltE m Salt

-- * Carrier

newtype CreateSaltC gen m a = CreateSaltC
  { runCreateSalt :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( DRG gen,
    Algebra sig m,
    Member (S.State gen) sig
  ) =>
  Algebra (CreateSaltE :+: sig) (CreateSaltC gen m)
  where
  alg _ (L CreateSalt) ctx = do
    g <- S.get @gen
    let (salt, g') = withDRG g $ newSalt <$> getRandomBytes 16
    S.put g'
    pure $ salt <$ ctx
  alg hdl (R other) ctx = CreateSaltC $ alg (runCreateSalt . hdl) other ctx
