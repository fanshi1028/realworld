{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Token.HasToken where

import Data.Domain (Domain (..))
import Data.Token.HasToken (TokenOf (..))
import Test.QuickCheck (Arbitrary)

deriving newtype instance Arbitrary (TokenOf 'User)
