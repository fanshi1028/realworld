{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Main where

import Domain.User (UserR)
import Domain.Util.JSON.To (Out)
import Test.Aeson.GenericSpecs (Proxy (Proxy), mkGoldenFileForType)

main :: IO ()
main = do
  putText "hi"
  -- mkGoldenFileForType 10 (Proxy @(Out (UserR "authWithToken"))) "golden"
