{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Storage.Map.HasStorage where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasStorage (ContentOf, IdOf (..))
import Gen.Naive.Data.Field.Body ()
import Gen.Naive.Data.Field.Description ()
import Gen.Naive.Data.Field.Time ()
import Gen.Naive.Data.Field.Title ()
import Gen.Naive.Data.Field.Username ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)
import Test.QuickCheck.Instances.UUID ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(IdOf 'User)
-- ["","","\1101124","5\EM\SO(\DC4\1013152","T","0\991668\\","3C8mx\DC2c\DC3","~\STX",";\a|Dy@@\\\ACK","\1007538K\bT\EOT3\DLE@\\\1033279\1097533o","[?\1051343O\1061543=\NULD\EM"]
deriving newtype instance Arbitrary (IdOf 'User)

deriving newtype instance Arbitrary (IdOf 'Comment)

instance Arbitrary (ContentOf 'Article) where
  arbitrary = genericArbitrary
  shrink = genericShrink
