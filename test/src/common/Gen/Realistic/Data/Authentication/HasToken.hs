{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Authentication.HasToken where

import Data.Authentication.HasToken (TokenOf (..))
import Data.Domain (Domain (..))
import Test.QuickCheck (Arbitrary)

deriving newtype instance Arbitrary (TokenOf 'User)
