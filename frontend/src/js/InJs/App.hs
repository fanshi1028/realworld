-- |
module InJs.App where

import Servant.Client (ClientEnv)

app :: ClientEnv -> IO ()
app _ = do
  putText "to be implemented!"
