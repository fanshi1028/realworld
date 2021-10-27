{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module Gen.Naive where

import Article (ArticleR (..))
import Authentication (AuthOf)
import Comment (CommentR (..))
import Domain (Domain (Article, Comment, User))
import Field.Bio (Bio (Bio))
import Field.Body (Body (Body))
import Field.Description (Description (Description))
import Field.Email (Email (Email))
import Field.Image (Image (Image))
import Field.Tag (Tag (Tag))
import Field.Title (Title (Title))
import Field.Username (Username (Username))
import Storage.Map (HasStorage (ContentOf), IdOf (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)
import Token (TokenOf (..))
import User (UserR (..))
import Util.JSON.To (Out (Out))

-- $setup
-- >>> import Test.QuickCheck (sample')

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Email
-- ["","\US\1045981","\GS","","_\1055675\53519XI+","\SUB_c\1067638(RD\t9","e\198568","G\65252r]\nD\185163w\1029797\ETXQ\92989\1091823\1022806","M","Y\172713\1069130\1053094r","~I\1019569d\CAN"]
deriving instance Arbitrary Email

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Username
-- ["",".","\DLE\tv\1034026","","\1011336\45919\31355/ CF\136841","0\176300\SUB\t\DC4uGb\1059101\&7","Cl\9463\&4\CAN\50560\1068046","\97256\1041103(\ACK,","\DLEA\a\EM{7\a^&\ACK\1054374\&4k\DC4\19325&","\n7m\r\1030889'\151710R|\1033552b>l\GS\151713r\SYN","6!\SOo\144149y\SYN\36267\&2vi%=By:\131925|\SO\ETX"]
deriving instance Arbitrary Username

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Bio
-- WAS ["","\39241\ETX","N","%]\v(\1036806R"," \SI;\35588ed/","|\46090\785*M\993095\1000056\ENQl~","\1108373v\73747\1061451\v>","+p\tW]\SIg%\STX8\996239","\44024@W\1001658\DC1\63969*m\SUB","H{]\171029[\r[m|","]7(\171557\SOH*\172850\tq\14088\27226z\1078432ToZW"]
-- NOW ["","Zu","2","n~3","\1061362\1069450\143147\1064262","i\DC2\SO\r)","QND\1032914:\1079725T\1099278\1000202\&1","\996221z\996661\131191\SI\SUB.","\155056{*b\137377Y\3865\GS\40074\NAK\1015609'\b","\1000569\132238h\147699\ACKOBs,1\1074427K\1033873\US","_B\1039948\1047857\128450\GS~>\49573,\9041\32894I"]
deriving instance Arbitrary Bio

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Image
-- ["","\v\168113","\ACK","","H\189844\ETB","","\DC2J|\SOH\t\SOQ\1103986\SYN%\188423","^\1101298\1102381{","E\a\SOH\ENQK","oa\157026~yglo\t{~\32949=","]"]
deriving instance Arbitrary Image

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Title
-- ["","\33532","\1082204\ESC","\ENQC}h-J","b","l\1086590\\","\78209:k\ESC","c","-~\1069271\&7n!\199009}\30571B","!\68081re\\\"Y~","\RSq;"]
deriving instance Arbitrary Title

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Description
-- ["","\v\ETX","\98655","","*j\1086030\24957,V",":","\98136\187630\1027868","\111228e\US&b4<}\aAfu","]T-kN]\DC1lmXz%","6\62613wyh\DC1\a\GS]t\GS\1106069r)","\CAN%\NUL\175015\28016w\DLEy\987128"]
deriving instance Arbitrary Description

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Body
-- ["","m\1104116","","\DLEE\134547","","\34015\152337fnh\ESC\FS","C(\STX::J\1084288^","\RSE$J*","\1055281\1070167b\21819K\SYN\187377\ACK:dt{FVA\SYN","\1092128","\201271:9."]
deriving instance Arbitrary Body

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @Tag
-- ["","N]","9\1012553","w\1016665\&5EK","Z3G!z\SOH+\1027100","\64959\DC3K\FSj\v\a\1039450@\1071227","\155137\&1\r","H\995424<Z\1105346\20322\EOT\DC4\39312`=","I\1031729\1008231uI","\ETBe>,k%\NAK\EOTE;8d\990470\78144","X-\DC2\60369\&6p(\DLEo=E\4320\DELl${["]
deriving instance Arbitrary Tag

deriving newtype instance Arbitrary a => Arbitrary (Out a)

-- deriving newtype instance Arbitrary a => Arbitrary (In a)

-- deriving newtype instance Eq a => Eq (Out a)

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(AuthOf 'User)
-- WAS [UserAuth {email = "", username = "", bio = "", image = ""},UserAuth {email = "\\6", username = "", bio = "", image = ""},UserAuth {email = "", username = "", bio = "\DC2\1047351\133765", image = "W\1052335\t"},UserAuth {email = "\DC2", username = "-}\1021226\&7\1000742", bio = "\37981B", image = "\147068\1011348mVa"},UserAuth {email = "Jyep\RS\1035404", username = "\1016070D\RS8\33119", bio = "", image = "P`\EMW\ENQu\1081175"},UserAuth {email = "H*\ESC\994817`g\DC2\7971l", username = "8\143363gB1\ENQ", bio = " {B8qip->/", image = "C4\a\1019800a\r\STX\ETB\FS\DC4"},UserAuth {email = "\180325\tRf\1083814nM)\53070\NUL\NAK%", username = "\121003\NUL\983155~\f\146994", bio = "\SOH=\NAK\SI4[n", image = "\v"},UserAuth {email = "\SOHCl1&W6\EOT_\43542tZ", username = "W6AJ`\1058933N\SUB\ENQ\171762", bio = "1E", image = "U"},UserAuth {email = "h N\DLE\175556\94730w\DC4\27137H\DC3\FSb", username = "\68341q#Y-\SO", bio = "*\CANx\DC3Z>\96679\&8\ENQ\DELAJ\154512\CAN\179281\FS", image = "9\ESCv\1008259\985854/wWVD\167471\"\1097135\bab"},UserAuth {email = "N\28505BSgeW\143097\991223\988131\78372", username = "", bio = ".i\995054\b\t\62203\&944'K\NAK", image = "\SYN5\181382J\51439\985649,(\1108388\&2\CANH3\178047"},UserAuth {email = ",\ACK^;R\1031043~\98762\172182cp7> \SIt`\EOT\nP", username = "L'M5\48254S\1104612", bio = "\DC1qPIL\143840\&1Q\US\RS\136164\1091341D\n\STXT@1\145541\DEL", image = "\GSZ+[:z\1061272YR"}]
-- NOW [UserAuth {email = "", username = "", bio = "", image = ""},UserAuth {email = "m", username = "", bio = "\1034693V", image = "x"},UserAuth {email = "b", username = "rr\7527%", bio = "", image = "\ETB"},UserAuth {email = "\EOTo", username = "\ETB\38848", bio = "\ESCb", image = "L6\1011038B\FS"},UserAuth {email = ".Jp\NUL~\1110304\60383", username = "\1025827P)\CAN\EOT", bio = "4\17109", image = "\1066602B"},UserAuth {email = "{", username = "", bio = ".Yod", image = ".\fBM\f8"},UserAuth {email = "\30657q\GS]ce\158630", username = "\62277\DLE\EOT\n\DC1c3\CANv\GS\a+", bio = "]O\6137|\1045324Dk;U", image = "d+\11650\&3"},UserAuth {email = "\135909vA\1097510\SOHP ", username = "%<KL2O\63358\183146", bio = "*\RS<\DEL\23624", image = "\a\FSI"},UserAuth {email = "\160937O~2\GS6\EM\1084986\ESCQ\178453L\197571", username = "e", bio = "\DEL\96775\194617W\rs\fV", image = "\FS"},UserAuth {email = "k\SYNK`\142934\1086926a1", username = "\US\1106295k\v8I_\vJ\1067314hQ\DC3\t^\DC3", bio = ")B\nkk\SYN+6\1081676\DLEfV*\1050176Y(\65179", image = "\b[<2"},UserAuth {email = "z=%S&[", username = "\146303\DC3x", bio = "CV\94405]t\1065825\&7u\1088738", image = "\SYNRlcr91\EOT\190289\178478\t:\1072911\ENQB<\SOHA"}]
instance Arbitrary (AuthOf 'User) where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(IdOf 'User)
-- ["","","\1101124","5\EM\SO(\DC4\1013152","T","0\991668\\","3C8mx\DC2c\DC3","~\STX",";\a|Dy@@\\\ACK","\1007538K\bT\EOT3\DLE@\\\1033279\1097533o","[?\1051343O\1061543=\NULD\EM"]
deriving newtype instance Arbitrary (IdOf 'User)

instance Arbitrary (ContentOf 'Article) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (UserR "profile") where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (ArticleR "withAuthorProfile") where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving newtype instance Arbitrary (IdOf 'Comment)

instance Arbitrary (CommentR "withAuthorProfile") where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving newtype instance Arbitrary (TokenOf 'User)

instance Arbitrary (UserR "authWithToken") where
  arbitrary = genericArbitrary
  shrink = genericShrink
