{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Authorization.TokenAuth (TokenAuth) where

import Authorization (E (AuthCheck))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R
import Control.Effect.Sum (Member)
import qualified Data.List as List
import Domain.User (UserR)
import GHC.TypeLits (Symbol)
import Network.Wai (requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings, verifyJWT)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))

newtype C (r :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Member (R.Reader JWTSettings) sig,
    MonadIO m
  ) =>
  Algebra (E UserR :+: sig) (C r m)
  where
  alg _ (L (AuthCheck req)) ctx = do
    mAuth <- case List.lookup "authorization" $ requestHeaders req of
      Just (parseHeader @(UserR "token") -> Right token) -> do
        jwts <- R.ask
        liftIO $ verifyJWT jwts $ show token
      _ -> pure Nothing
    pure $ maybe mempty pure mAuth <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

data TokenAuth

instance IsAuth TokenAuth (UserR "id") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ cs jwts =
    Auth.AuthCheck $
      runM
        . R.runReader cs
        . R.runReader jwts
        . run
        . send
        . AuthCheck @UserR
