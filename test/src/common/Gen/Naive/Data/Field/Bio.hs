{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Bio where

import Data.Field.Bio (Bio (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Bio
-- WAS ["","\39241\ETX","N","%]\v(\1036806R"," \SI;\35588ed/","|\46090\785*M\993095\1000056\ENQl~","\1108373v\73747\1061451\v>","+p\tW]\SIg%\STX8\996239","\44024@W\1001658\DC1\63969*m\SUB","H{]\171029[\r[m|","]7(\171557\SOH*\172850\tq\14088\27226z\1078432ToZW"]
-- NOW ["","Zu","2","n~3","\1061362\1069450\143147\1064262","i\DC2\SO\r)","QND\1032914:\1079725T\1099278\1000202\&1","\996221z\996661\131191\SI\SUB.","\155056{*b\137377Y\3865\GS\40074\NAK\1015609'\b","\1000569\132238h\147699\ACKOBs,1\1074427K\1033873\US","_B\1039948\1047857\128450\GS~>\49573,\9041\32894I"]
deriving instance Arbitrary Bio
