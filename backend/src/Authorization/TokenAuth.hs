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
import Data.ByteString.Base64.Type (getBS64)
import qualified Data.List as List
import Domain.User (UserR (Token, UserAuthWithToken))
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
    auth <- case List.lookup "authorization" $ requestHeaders req of
      Just (parseHeader @(UserR "token") -> Right t@(Token token)) -> do
        jwts <- R.ask
        liftIO (verifyJWT jwts $ getBS64 token) >>= \case
          Nothing -> pure mempty
          Just auth -> pure $ pure $ UserAuthWithToken auth t
      _ -> pure mempty
    pure $ auth <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

data TokenAuth

instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ _ jwts =
    Auth.AuthCheck $
      runM
        . R.runReader jwts
        . run
        . send
        . AuthCheck @UserR
