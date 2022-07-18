{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | @since 0.2.0.0
module Gen.Realistic (module Gen.Realistic.Util) where

import Gen.Realistic.Data.Authentication.HasAuth ()
import Gen.Realistic.Data.Storage.Map.HasCreate ()
import Gen.Realistic.Data.Storage.Map.HasUpdate ()
import Gen.Realistic.Util (Realistic (..), arbitraryRealistic, shrinkRealistic)
