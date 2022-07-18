{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Body where

import Data.Field.Body (Body (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Body
-- ["","m\1104116","","\DLEE\134547","","\34015\152337fnh\ESC\FS","C(\STX::J\1084288^","\RSE$J*","\1055281\1070167b\21819K\SYN\187377\ACK:dt{FVA\SYN","\1092128","\201271:9."]
deriving instance Arbitrary Body
