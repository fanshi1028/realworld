-- |
module Main where

import App.InMem (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (withApplication)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Test.Tasty (testGroup)
import StateMachine (prop1, prop2)
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
      [
        specs,
        testProperties "state machine: " $
          [ ("seq", prop1 newApp mgr mkUrl),
            ("parallel", prop2 newApp mgr mkUrl)
          ]
      ]
