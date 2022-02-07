{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module InMem.StateMachine (inMemAppStateMachinePropTest) where

import InMem.App (newApp)
import Network.HTTP.Client (Manager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client (BaseUrl, mkClientEnv)
import StateMachine (stateMachine)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.Monadic (monadic)
import Test.StateMachine
  ( Reason (Ok),
    checkCommandNames,
    coverCommandNames,
    forAllCommands,
    prettyCommands,
    runCommands,
  )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, (===))

checkInMemAppProp :: Manager -> (Int -> BaseUrl) -> Property
checkInMemAppProp mgr mkUrl =
  forAllCommands stateMachine Nothing $ \cmds -> monadic
    ( ioProperty
        . \prop -> testWithApplication newApp $ \port -> usingReaderT (mkClientEnv mgr $ mkUrl port) prop
    )
    $ do
      (hist, _, res) <- runCommands stateMachine cmds
      prettyCommands stateMachine hist $ coverCommandNames cmds $ checkCommandNames cmds $ res === Ok

inMemAppStateMachinePropTest :: Manager -> (Int -> BaseUrl) -> TestTree
inMemAppStateMachinePropTest mgr mkUrl = testProperty "In mem app (seq)" $ checkInMemAppProp mgr mkUrl
