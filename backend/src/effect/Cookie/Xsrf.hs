{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of creating xsrf cookie
--
-- @since 0.4.0.0
module Cookie.Xsrf where

import Control.Algebra (alg, type (:+:) (L, R))
import Control.Effect.Lift (Algebra)
import qualified Control.Effect.Reader as R (Reader, ask)
import qualified Control.Effect.State as S (State, get, put)
import Control.Effect.Sum (Member)
import Crypto.Random (DRG, getRandomBytes, withDRG)
import Servant.Auth.Server (CookieSettings, cookieXsrfSetting)
import Servant.Auth.Server.Internal.Cookie (applyCookieSettings, applyXsrfCookieSettings, noXsrfTokenCookie)
import Web.Cookie (SetCookie, def, setCookieValue)

-- * Effect

-- | @since 0.3.0.0
data CreateXsrfCookieE (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Create xsrf cookie
  CreateXsrfCookie :: CreateXsrfCookieE m SetCookie

-- * Carrier

-- | @since 0.3.0.0
newtype CreateXsrfCookieC gen m a = CreateXsrfCookieC
  { -- | @since 0.3.0.0
    runCreateXsrfCookie :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
-- Copy from servant auth but amended so that it is not in IO
makeXsrfCookie :: DRG gen => CookieSettings -> gen -> (SetCookie, gen)
makeXsrfCookie cookieSettings gen = case cookieXsrfSetting cookieSettings of
  Just xsrfCookieSettings -> withDRG gen $ do
    xsrfValue <- getRandomBytes 32
    pure $
      applyXsrfCookieSettings xsrfCookieSettings $
        applyCookieSettings cookieSettings $
          def {setCookieValue = xsrfValue}
  Nothing -> (noXsrfTokenCookie cookieSettings, gen)
{-# INLINE makeXsrfCookie #-}

-- | @since 0.3.0.0
instance
  ( Algebra sig m,
    Member (R.Reader CookieSettings) sig,
    Member (S.State gen) sig,
    DRG gen
  ) =>
  Algebra (CreateXsrfCookieE :+: sig) (CreateXsrfCookieC gen m)
  where
  alg _ (L CreateXsrfCookie) ctx = do
    cookieSettings <- R.ask
    (xsrfCookie, g) <- makeXsrfCookie cookieSettings <$> S.get @gen
    S.put g
    pure $ xsrfCookie <$ ctx
  alg hdl (R other) ctx = CreateXsrfCookieC $ alg (runCreateXsrfCookie . hdl) other ctx
  {-# INLINE alg #-}
