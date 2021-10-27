-- |
module Main where

import App.InMem (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import StateMachine (prop1)
import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (testProperties)

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
        testProperties "state machine: " $
          [ ("seq", prop1 newApp mgr mkUrl)
          -- NOTE: current state machine is not build for concurrency
          -- ("parallel", prop2 newApp mgr mkUrl)
          ]
      ]
