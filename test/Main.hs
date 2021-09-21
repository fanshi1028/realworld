-- |
module Main where

import Roundtrip (aesonRoundtripTests)
import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)

-- * Main

-- | @since 0.2.0.0
--
-- run tests
main :: IO ()
main = do
  specs <- aesonRoundtripTests
  defaultMainWithRerun $ testGroup "tests" [specs]
