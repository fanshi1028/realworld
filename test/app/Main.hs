{-# LANGUAGE CPP #-}

-- |
module Main where

import Roundtrip (aesonRoundtripTests)
import Test.Tasty (localOption, testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (QuickCheckReplay (QuickCheckReplay))

#if backendInMem || backendRel8
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
#if backendInMem
import InMem.StateMachine (inMemAppStateMachinePropTest)
#elif backendRel8
import InRel8.StateMachine (inRel8AppStateMachinePropTest)
#endif
#endif

-- * Main

-- | run tests
main :: IO ()
main = do
  aesonPropsTest <- aesonRoundtripTests
#if backendInMem || backendRel8
  mgr <- newManager defaultManagerSettings
  let mkUrl port = BaseUrl Http "localhost" port ""
#endif
  defaultMainWithRerun . testGroup "tests" $
    [ aesonPropsTest
#if backendInMem || backendRel8
      , localOption (QuickCheckReplay Nothing) . testGroup "state machine:" $
        (\mkTest -> mkTest mgr mkUrl)
          <$>
#if backendInMem
                [ inMemAppStateMachinePropTest ]
#elif backendRel8
                [ inRel8AppStateMachinePropTest ]
#endif
#endif
    ]
