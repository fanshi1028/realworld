{-# LANGUAGE CPP #-}

-- |
module Main where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Test.Tasty (localOption, testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (QuickCheckReplay (QuickCheckReplay))

#if backendInMem
import InMem.StateMachine (inMemAppStateMachinePropTest)
#endif

#if backendRel8
import InRel8.StateMachine (inRel8AppStateMachinePropTest)
#endif

-- * Main

-- | run tests
main :: IO ()
main = do
  aesonPropsTest <- aesonRoundtripTests
  mgr <- newManager defaultManagerSettings
  let mkUrl port = BaseUrl Http "localhost" port ""
  defaultMainWithRerun . testGroup "tests" $
    [ aesonPropsTest,
      localOption (QuickCheckReplay Nothing) . testGroup "state machine:" $
        (\mkTest -> mkTest mgr mkUrl)
          <$> (
#if backendInMem && backendRel8
                inMemAppStateMachinePropTest : inRel8AppStateMachinePropTest :
#elif backendInMem
                inMemAppStateMachinePropTest :
#elif backendRel8
                inRel8AppStateMachinePropTest :
#endif
                []
              )
    ]
