{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- |
module Main where

import Database.Postgres.Temp (DirectoryType (Temporary), cacheDirectoryType, cleanupInitDbCache, defaultCacheConfig, setupInitDbCache)
import InMem.App (newApp)
import qualified InRel8.App as InRel8 (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import StateMachine (checkInMemAppProp, checkRel8AppProp)
import Test.Tasty (localOption, testGroup, withResource)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (QuickCheckReplay (QuickCheckReplay), testProperty)

-- * Main

-- | @since 0.2.0.0
--
-- run tests
main :: IO ()
main = do
  specs <- aesonRoundtripTests
  mgr <- newManager defaultManagerSettings
  let mkUrl port = BaseUrl Http "localhost" port ""
  defaultMainWithRerun $
    testGroup
      "tests"
      [ specs,
        localOption (QuickCheckReplay Nothing) $
          testGroup
            "state machine:"
            [ testProperty "In mem app (seq)" $ checkInMemAppProp newApp mgr mkUrl,
              withResource (setupInitDbCache defaultCacheConfig {cacheDirectoryType = Temporary}) cleanupInitDbCache $ \getDbCache ->
                testProperty "Rel8 app (seq)" $ checkRel8AppProp getDbCache InRel8.newApp mgr mkUrl
            ]
      ]
