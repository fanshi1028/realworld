{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Storage.Map.HasId where

import Data.Domain (Domain (..))
import Data.Storage.Map (IdOf (..))
import Gen.Naive.Data.Field.Username ()
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.UUID ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(IdOf 'User)
-- ["","","\1101124","5\EM\SO(\DC4\1013152","T","0\991668\\","3C8mx\DC2c\DC3","~\STX",";\a|Dy@@\\\ACK","\1007538K\bT\EOT3\DLE@\\\1033279\1097533o","[?\1051343O\1061543=\NULD\EM"]
deriving newtype instance Arbitrary (IdOf 'User)

deriving newtype instance Arbitrary (IdOf 'Comment)
