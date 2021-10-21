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
-- Carrier in pure
--
-- @since 0.1.0.0
module Current.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Current (E (GetCurrent))
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Field.Time (Time)

-- | @since 0.1.0.0
newtype C e m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.2.0.0
instance (Algebra sig m) => Algebra (E Time :+: sig) (C Time m) where
  alg _ (L GetCurrent) ctx = pure $ UTCTime (ModifiedJulianDay 0) 0 <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
