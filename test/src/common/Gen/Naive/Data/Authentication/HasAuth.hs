{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Authentication.HasAuth where

import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (..))
import Gen.Naive.Data.Field.Bio ()
import Gen.Naive.Data.Field.Email ()
import Gen.Naive.Data.Field.Image ()
import Gen.Naive.Data.Field.Username ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(AuthOf 'User)
-- WAS [UserAuth {email = "", username = "", bio = "", image = ""},UserAuth {email = "\\6", username = "", bio = "", image = ""},UserAuth {email = "", username = "", bio = "\DC2\1047351\133765", image = "W\1052335\t"},UserAuth {email = "\DC2", username = "-}\1021226\&7\1000742", bio = "\37981B", image = "\147068\1011348mVa"},UserAuth {email = "Jyep\RS\1035404", username = "\1016070D\RS8\33119", bio = "", image = "P`\EMW\ENQu\1081175"},UserAuth {email = "H*\ESC\994817`g\DC2\7971l", username = "8\143363gB1\ENQ", bio = " {B8qip->/", image = "C4\a\1019800a\r\STX\ETB\FS\DC4"},UserAuth {email = "\180325\tRf\1083814nM)\53070\NUL\NAK%", username = "\121003\NUL\983155~\f\146994", bio = "\SOH=\NAK\SI4[n", image = "\v"},UserAuth {email = "\SOHCl1&W6\EOT_\43542tZ", username = "W6AJ`\1058933N\SUB\ENQ\171762", bio = "1E", image = "U"},UserAuth {email = "h N\DLE\175556\94730w\DC4\27137H\DC3\FSb", username = "\68341q#Y-\SO", bio = "*\CANx\DC3Z>\96679\&8\ENQ\DELAJ\154512\CAN\179281\FS", image = "9\ESCv\1008259\985854/wWVD\167471\"\1097135\bab"},UserAuth {email = "N\28505BSgeW\143097\991223\988131\78372", username = "", bio = ".i\995054\b\t\62203\&944'K\NAK", image = "\SYN5\181382J\51439\985649,(\1108388\&2\CANH3\178047"},UserAuth {email = ",\ACK^;R\1031043~\98762\172182cp7> \SIt`\EOT\nP", username = "L'M5\48254S\1104612", bio = "\DC1qPIL\143840\&1Q\US\RS\136164\1091341D\n\STXT@1\145541\DEL", image = "\GSZ+[:z\1061272YR"}]
-- NOW [UserAuth {email = "", username = "", bio = "", image = ""},UserAuth {email = "m", username = "", bio = "\1034693V", image = "x"},UserAuth {email = "b", username = "rr\7527%", bio = "", image = "\ETB"},UserAuth {email = "\EOTo", username = "\ETB\38848", bio = "\ESCb", image = "L6\1011038B\FS"},UserAuth {email = ".Jp\NUL~\1110304\60383", username = "\1025827P)\CAN\EOT", bio = "4\17109", image = "\1066602B"},UserAuth {email = "{", username = "", bio = ".Yod", image = ".\fBM\f8"},UserAuth {email = "\30657q\GS]ce\158630", username = "\62277\DLE\EOT\n\DC1c3\CANv\GS\a+", bio = "]O\6137|\1045324Dk;U", image = "d+\11650\&3"},UserAuth {email = "\135909vA\1097510\SOHP ", username = "%<KL2O\63358\183146", bio = "*\RS<\DEL\23624", image = "\a\FSI"},UserAuth {email = "\160937O~2\GS6\EM\1084986\ESCQ\178453L\197571", username = "e", bio = "\DEL\96775\194617W\rs\fV", image = "\FS"},UserAuth {email = "k\SYNK`\142934\1086926a1", username = "\US\1106295k\v8I_\vJ\1067314hQ\DC3\t^\DC3", bio = ")B\nkk\SYN+6\1081676\DLEfV*\1050176Y(\65179", image = "\b[<2"},UserAuth {email = "z=%S&[", username = "\146303\DC3x", bio = "CV\94405]t\1065825\&7u\1088738", image = "\SYNRlcr91\EOT\190289\178478\t:\1072911\ENQB<\SOHA"}]
instance Arbitrary (AuthOf 'User) where
  arbitrary = genericArbitrary
  shrink = genericShrink
