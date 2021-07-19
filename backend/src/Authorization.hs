{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Authorization (TokenAuth, TokenAuthInMem) where

import Data.ByteString.Base64.Type (getBS64)
import qualified Data.List as List
import Domain.User (UserR (Token, UserAuthWithToken))
import Domain.Util.Representation (Transform (transform))
import Network.Wai (requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings, verifyJWT)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM
import Storage.InMem (TableInMem, TableInMem')

data TokenAuth

instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ _ jwts = Auth.AuthCheck $
    \case
      ( List.lookup "authorization" . requestHeaders ->
          Just (parseHeader @(UserR "token") -> Right t@(Token token))
        ) -> maybe mempty (pure . flip UserAuthWithToken t) <$> liftIO (verifyJWT jwts $ getBS64 token)
      _ -> pure mempty

data TokenAuthInMem

instance IsAuth TokenAuthInMem (UserR "auth") where
  type AuthArgs TokenAuthInMem = '[TableInMem UserR, TableInMem' UserR "token" "id"]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    ( List.lookup "authorization" . requestHeaders ->
        Just (parseHeader @(UserR "token") -> Right token)
      ) ->
        atomically
          ( STM.lookup token tokenDb
              >>= traverse (`STM.lookup` userDb)
              <&> maybe mempty (pure . transform @_ @_ @"auth") . join
          )
    _ -> pure mempty
