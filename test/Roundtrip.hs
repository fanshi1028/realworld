{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module Roundtrip (aesonRoundtripTests) where

import Authentication.HasAuth (LoginOf)
import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Typeable (typeRep)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Field.Tag (Tag)
import Field.Time (Time)
import Gen.Naive ()
import Gen.Realistic (Realistic (Realistic))
import Orphans ()
import Storage.Map (CreateOf, IdOf, Patch, UpdateOf)
import Test.Aeson.GenericSpecs (defaultSettings, roundtripAndGoldenSpecsWithSettings, roundtripSpecs, sampleSize)
import Test.Aeson.Internal.Utils (addBrackets)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, counterexample, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpecs)
import Token.HasToken (TokenOf)
import Util.JSON.From (In (In))
import Util.JSON.To (Out)
import Util.Validation (WithValidation)
import Validation (Validation (Failure, Success))

-- | FIXME realistic validated instance?
roundtripSpecsWithValidation ::
  forall a.
  ( Typeable a,
    Eq a,
    Show a,
    ToJSON a,
    FromJSON (WithValidation a),
    ToJSON (In a),
    FromJSON (In (WithValidation a)),
    Arbitrary (Realistic a)
  ) =>
  Proxy a ->
  Spec
roundtripSpecsWithValidation proxy =
  let describe' info =
        describe ("JSON encoding of " ++ addBrackets info)
          . prop "allows to encode values with aeson and read them back with validation"
      helper1 :: Realistic a -> Property
      helper1 (Realistic a) = case eitherDecode' @(WithValidation _) (encode a) of
        Right (Success r) -> ((===) `on` encode) a r
        Right (Failure err) -> counterexample (show err) False
        Left err -> counterexample err False
      helper2 :: Realistic a -> Property
      helper2 (Realistic a) = case eitherDecode' @(In (WithValidation _)) (encode $ In a) of
        Right (In (Success r)) -> ((===) `on` encode . In) a r
        Right (In (Failure err)) -> counterexample (show err) False
        Left err -> counterexample err False
   in do
        describe' (show $ typeRep proxy) helper1
        describe' ("In " <> addBrackets (show $ typeRep proxy)) helper2

-- | @since 0.2.0.0
--
-- roundtrip specs for simple type
simpleRoundtripSpecs :: Spec
simpleRoundtripSpecs = do
  roundtripSpecs $ Proxy @Tag
  roundtripSpecs $ Proxy @Time
  -- NOTE: Slug is drived from Title, not need to test it?
  -- roundtripSpecs $ Proxy @Slug
  -- roundtripSpecs $ Proxy @(IdOf 'Article)
  roundtripSpecs $ Proxy @(IdOf 'User)
  roundtripSpecs $ Proxy @(IdOf 'Comment)
  roundtripSpecs $ Proxy @(TokenOf 'User)
  roundtripSpecs $ Proxy @(UserR "authWithToken")

-- | @since 0.2.0.0
--
-- Roundtrip specs with Validation
inRoundtripSpecs :: Spec
inRoundtripSpecs = do
  roundtripSpecsWithValidation $ Proxy @(LoginOf 'User)
  roundtripSpecsWithValidation $ Proxy @(CreateOf 'User)
  roundtripSpecsWithValidation $ Proxy @(CreateOf 'Article)
  roundtripSpecsWithValidation $ Proxy @(CreateOf 'Comment)
  roundtripSpecsWithValidation $ Proxy @(Patch (UpdateOf 'User))
  roundtripSpecsWithValidation $ Proxy @(Patch (UpdateOf 'Article))

-- | @since 0.2.0.0
--
-- Roundtrip specs for out type
outRoundtripSpecs :: Spec
outRoundtripSpecs = do
  let roundtripAndGoldenSpecs' p = roundtripAndGoldenSpecsWithSettings defaultSettings {sampleSize = 30} p
  roundtripAndGoldenSpecs' $ Proxy @(UserR "authWithToken")
  roundtripAndGoldenSpecs' $ Proxy @(Out (UserR "authWithToken"))
  roundtripAndGoldenSpecs' $ Proxy @(UserR "profile")
  roundtripAndGoldenSpecs' $ Proxy @(Out (UserR "profile"))
  roundtripAndGoldenSpecs' $ Proxy @(ArticleR "withAuthorProfile")
  roundtripAndGoldenSpecs' $ Proxy @(Out (ArticleR "withAuthorProfile"))
  roundtripAndGoldenSpecs' $ Proxy @(Out [ArticleR "withAuthorProfile"])
  roundtripAndGoldenSpecs' $ Proxy @(CommentR "withAuthorProfile")
  roundtripAndGoldenSpecs' $ Proxy @(Out (CommentR "withAuthorProfile"))
  roundtripAndGoldenSpecs' $ Proxy @(Out [CommentR "withAuthorProfile"])
  roundtripAndGoldenSpecs' $ Proxy @(Out [Tag])

aesonRoundtripTests :: IO TestTree
aesonRoundtripTests = do
  testGroup "aeson"
    <$> sequence
      [ testGroup "aeson-roundtrip"
          <$> testSpecs
            ( do
                simpleRoundtripSpecs
                inRoundtripSpecs
            ),
        testGroup "aeson-golden-roundtrip" <$> testSpecs outRoundtripSpecs
      ]
