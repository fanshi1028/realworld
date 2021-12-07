-- |
module Main where

import InMem.App (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import StateMachine (prop1)
import Test.Tasty (localOption, testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (QuickCheckReplay (QuickCheckReplay), testProperties)

-- * Main

-- | @since 0.2.0.0
--
-- run tests
main :: IO ()
main = do
  let mkUrl port = BaseUrl Http "localhost" port ""
  specs <- aesonRoundtripTests
  mgr <- newManager defaultManagerSettings
  defaultMainWithRerun $
    testGroup
      "tests"
      [ specs,
        localOption (QuickCheckReplay Nothing) $
          testProperties
            "state machine: "
            [ ("seq", prop1 newApp mgr mkUrl)
            -- NOTE: current state machine is not build for concurrency
            -- ("parallel", prop2 newApp mgr mkUrl)
            ]
      ]
