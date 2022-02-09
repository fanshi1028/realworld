{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Description : App
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- App with postgres storage
--
-- @since 0.4.0.0
module InRel8.App where

import API (Api)
import qualified Control.Carrier.Error.Church as Church (runError)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import qualified Control.Carrier.State.Strict as S (evalState)
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Returning (runTrace)
import Control.Effect.Lift (sendM)
import qualified Control.Effect.Throw as T (throwError)
import Control.Exception.Safe (bracket, catch)
import qualified Crypto.JOSE (Error)
import Crypto.Random (SystemDRG, getSystemDRG)
import Data.Authentication.HasAuth (AlreadyLogin, AuthOf, NotAuthorized, NotLogin)
import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Email (Email)
import Data.Field.Tag (Tag)
import Data.Paging (Limit (Limit), Offset (Offset))
import Data.Storage.Error (AlreadyExists, NotFound)
import Data.Storage.Map (CRUD (D, U), Forbidden, IdAlreadyExists, IdNotFound)
import Data.Token.HasToken (TokenOf (..))
import Data.UUID.V4 (nextRandom)
import Data.Util.Impossible (Impossible)
import Data.Util.Validation (ValidationErr)
import Effect.Cookie.Xsrf (runCreateXsrfCookie)
import Effect.CreateSalt (runCreateSalt)
import Effect.Token.Create.JWT (runCreateTokenJWT)
import Effect.Token.Decode (InvalidToken)
import Hasql.Connection (acquire, release)
import qualified Hasql.Session as Session (run)
import Hasql.Transaction (condemn)
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)
import InRel8.Authentication.User (runAuthenticationUserInRel8)
import InRel8.OptionalAuthAction (runOptionalAuthActionInRel8)
import InRel8.OptionalAuthAction.Many (OptionalAuthActionManyInRel8C (runOptionalAuthActionManyInRel8))
import InRel8.Sql (SqlInRel8TransactionC (runSqlInRel8Transaction))
import InRel8.UserAction (runUserActionInRel8)
import InRel8.UserAction.Many (runUserActionManyInRel8)
import InRel8.VisitorAction (VisitorActionInRel8C (runVisitorActionInRel8))
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    ServerError,
    err400,
    err401,
    err403,
    err404,
    err500,
    hoistServerWithContext,
    serveWithContext,
    throwError,
  )
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Server (errBody)
import Server (server)

-- | @since 0.4.0.0
asStatus :: Show e => (ServerError -> e -> ServerError)
asStatus status e = status {errBody = show e}

-- | @since 0.4.0.0
-- Error runner to throw in memory as 'ServerError' with HTTP status code
runErrors =
  let rollback f e = do
        sendM condemn
        T.throwError $ f e
      runThrowInRel8 f = runThrow >=> either (rollback f) pure
      runErrorInRel8 f = Church.runError (rollback f) pure
   in runThrowInRel8 (\e -> err500 {errBody = encodeUtf8 @Text e})
        >>> runErrorInRel8 (asStatus @(Forbidden 'D 'Article) err403)
        >>> runErrorInRel8 (asStatus @(Forbidden 'U 'Article) err403)
        >>> runErrorInRel8 (asStatus @(Forbidden 'D 'Comment) err403)
        >>> runErrorInRel8 (asStatus @(NotAuthorized 'User) err403)
        >>> runErrorInRel8 (asStatus @(NotLogin 'User) err401)
        >>> runErrorInRel8 (asStatus @(InvalidToken 'User) err401)
        >>> runErrorInRel8 (asStatus @(IdNotFound 'Article) err404)
        >>> runErrorInRel8 (asStatus @(IdNotFound 'Comment) err404)
        >>> runErrorInRel8 (asStatus @(IdNotFound 'User) err404)
        >>> runErrorInRel8 (asStatus @(NotFound Email) err404)
        >>> runThrowInRel8 (asStatus @(NotFound Tag) err404)
        >>> runThrowInRel8 (asStatus @(IdAlreadyExists 'User) err400)
        >>> runThrowInRel8 (asStatus @(IdAlreadyExists 'Article) err400)
        >>> runThrowInRel8 (asStatus @(IdAlreadyExists 'Comment) err500)
        >>> runThrowInRel8 (asStatus @(AlreadyExists Email) err400)
        >>> runThrowInRel8 (asStatus @(AlreadyLogin 'User) err400)
        >>> runThrowInRel8 (asStatus @ValidationErr err400)
        >>> runThrowInRel8 (asStatus @Crypto.JOSE.Error err500)
        >>> runThrowInRel8 (asStatus @Impossible err500)
{-# INLINE runErrors #-}

-- | @since 0.4.0.0
mkApp :: CookieSettings -> JWTSettings -> ByteString -> Application
mkApp cs jwts connStr =
  serveWithContext (Proxy @Api) (cs :. jwts :. EmptyContext) $
    hoistServerWithContext
      (Proxy @Api)
      (Proxy @'[CookieSettings, JWTSettings])
      ( \eff ->
          bracket
            ( liftIO (acquire connStr) >>= \case
                Left (fromMaybe "connection error" -> err) -> throwError $ err500 {errBody = fromStrict err}
                Right conn -> pure conn
            )
            (liftIO . release)
            ( \conn -> liftIO $ do
                uuid <- nextRandom
                gen <- getSystemDRG
                let runStorageInRel8 =
                      runVisitorActionInRel8 @[]
                        >>> runUserActionManyInRel8 @[]
                        >>> runUserActionInRel8
                        >>> runOptionalAuthActionManyInRel8 @[]
                        >>> runOptionalAuthActionInRel8
                        >>> runAuthenticationUserInRel8
                ( eff
                    & runStorageInRel8
                    & runCreateTokenJWT @'User @SystemDRG
                    & runCreateXsrfCookie @SystemDRG
                    & runCreateSalt @SystemDRG
                    & S.evalState gen
                    & R.runReader uuid
                    & R.runReader jwts
                    & R.runReader cs
                    & R.runReader (Limit 20)
                    & R.runReader (Offset 0)
                    & R.runReader (Nothing @(AuthOf 'User))
                    & R.runReader (Nothing @(TokenOf 'User))
                    & runErrors
                    & runError @ServerError
                    & runSqlInRel8Transaction
                    & runTrace
                    & runM
                  )
                  & transaction ReadCommitted Write
                  <&> snd
                  -- >>= ( \(_e, r) -> do
                  --         liftIO $ mapM_ putStrLn e
                  --         liftIO $ withFile "sql_log" AppendMode $ \h -> mapM_ (hPutStrLn h) e
                  --         pure r
                  --     )
                  & flip Session.run conn
            )
            >>= either (throwError . asStatus err500) pure
            >>= either throwError pure
            & (`catch` throwError)
      )
      server

-- | @since 0.4.0.0
newApp :: ByteString -> IO Application
newApp connStr = mkApp defaultCookieSettings . defaultJWTSettings <$> generateKey ?? connStr
