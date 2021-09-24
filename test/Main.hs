-- |
module Main where

import App.InMem (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (withApplication)
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
  let url port = BaseUrl Http "localhost/api" port ""
  specs <- aesonRoundtripTests
  withApplication newApp $ \port -> do
    mgr <- newManager defaultManagerSettings
    defaultMainWithRerun $
      testGroup
        "tests"
        [ specs,
          testProperties "state machine: " $
            [ ("seq", prop1 mgr $ url port)
            -- ("parallel", prop2 mgr $ url port)
            ]
        ]
