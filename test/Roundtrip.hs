{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module Roundtrip (aesonRoundtripTests) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Typeable (typeRep)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Domain.Util.Field (Tag, Time)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out)
import Domain.Util.Validation (WithValidation)
import Gen.Naive ()
import Gen.Realistic (Realistic (Realistic))
import Orphans ()
import Test.Aeson.GenericSpecs (defaultSettings, roundtripAndGoldenADTSpecsWithSettings, roundtripAndGoldenSpecsWithSettings, roundtripSpecs, sampleSize)
import Test.Aeson.Internal.Utils (addBrackets)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, counterexample, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpecs)
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
        -- Right (Failure _) -> discard
        Right (Failure err) -> counterexample (show err) False
        Left err -> counterexample err False
      helper2 :: Realistic a -> Property
      helper2 (Realistic a) = case eitherDecode' @(In (WithValidation _)) (encode $ In a) of
        Right (In (Success r)) -> ((===) `on` encode . In) a r
        -- Right (In (Failure _)) -> discard
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
  -- NOTE: Slug is drived from Title, not need to test it? So do (AritcleR "id")
  -- roundtripSpecs $ Proxy @Slug
  -- roundtripSpecs $ Proxy @(AritcleR "id")
  roundtripSpecs $ Proxy @(UserR "id")
  roundtripSpecs $ Proxy @(CommentR "id")
  roundtripSpecs $ Proxy @(UserR "token")
  roundtripSpecs $ Proxy @(UserR "authWithToken")

-- | @since 0.2.0.0
--
-- Roundtrip specs with Validation
inRoundtripSpecs :: Spec
inRoundtripSpecs = do
  roundtripSpecsWithValidation $ Proxy @(UserR "login")
  roundtripSpecsWithValidation $ Proxy @(UserR "create")
  roundtripSpecsWithValidation $ Proxy @(ArticleR "create")
  roundtripSpecsWithValidation $ Proxy @(CommentR "create")

-- roundtripSpecsWithValidation $ Proxy @(UserR "update")

-- | @since 0.2.0.0
--
-- Roundtrip specs for simple type
outRoundtripSpecs :: Spec
outRoundtripSpecs = do
  let roundtripAndGoldenSpecs' p = roundtripAndGoldenSpecsWithSettings defaultSettings {sampleSize = 30} p
  roundtripAndGoldenSpecs' $ Proxy @(Out (UserR "authWithToken"))
  roundtripAndGoldenSpecs' $ Proxy @(Out (UserR "profile"))
  roundtripAndGoldenSpecs' $ Proxy @(Out (ArticleR "withAuthorProfile"))
  roundtripAndGoldenSpecs' $ Proxy @(Out [ArticleR "withAuthorProfile"])
  roundtripAndGoldenSpecs' $ Proxy @(Out (CommentR "withAuthorProfile"))
  roundtripAndGoldenSpecs' $ Proxy @(Out [CommentR "withAuthorProfile"])
  roundtripAndGoldenSpecs' $ Proxy @(Out [Tag])

-- | @since 0.2.0.0
goldenAesonRoundtripTests :: IO TestTree
goldenAesonRoundtripTests = do
  let roundtripAndGoldenADTSpecs' p = roundtripAndGoldenADTSpecsWithSettings defaultSettings {sampleSize = 30} p
  testGroup "golden-aeson-adt-roundtrip"
    <$> testSpecs
      ( do
          roundtripAndGoldenADTSpecs' $ Proxy @(UserR "auth")
          roundtripAndGoldenADTSpecs' $ Proxy @(ArticleR "all")
          roundtripAndGoldenADTSpecs' $ Proxy @(UserR "profile")
          roundtripAndGoldenADTSpecs' $ Proxy @(CommentR "withAuthorProfile")
          roundtripAndGoldenADTSpecs' $ Proxy @(UserR "authWithToken")
          -- FIXME: title and slug are isomorphic, so the derived toADTArbitrary instance can't round trip.
          -- roundtripAndGoldenADTSpecs $ Proxy @(ArticleR "withAuthorProfile")
      )

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
        testGroup "aeson-golden-roundtrip" <$> testSpecs outRoundtripSpecs,
        goldenAesonRoundtripTests
      ]
