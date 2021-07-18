{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Authorization.TokenAuth.InMem (TokenAuthInMem) where

import Authorization (E (AuthCheck))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.Lift (runM)
import Control.Effect.Sum (Member)
import qualified Data.List as List
import Domain.User (UserR)
import GHC.TypeLits (Symbol)
import Network.Wai (requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import Storage.InMem (TableInMem')
import qualified Storage.Index as Index (E (GetByIndex))
import qualified Storage.Index.InMem

newtype C (r :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Member (Index.E UserR "token" Maybe) sig
  ) =>
  Algebra (E UserR :+: sig) (C r m)
  where
  alg _ (L (AuthCheck req)) ctx = do
    muid <- case List.lookup "authorization" $ requestHeaders req of
      Just (parseHeader @(UserR "token") -> Right token) -> send $ Index.GetByIndex @UserR @"token" @Maybe token
      _ -> pure Nothing
    pure $ maybe mempty pure muid <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

data TokenAuthInMem

instance IsAuth TokenAuthInMem (UserR "id") where
  type AuthArgs TokenAuthInMem = '[TableInMem' UserR "token" "id"]
  runAuth _ _ tokenDb =
    Auth.AuthCheck $
      runM
        . flip runReaderT tokenDb
        . Storage.Index.InMem.run @UserR @"token" @Maybe
        . run
        . send
        . AuthCheck @UserR
