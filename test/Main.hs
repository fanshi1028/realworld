{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- |
module Main where

import Control.Exception.Safe (bracket)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationCommands, MigrationFile, MigrationInitialization, MigrationValidation), MigrationOptions (optVerbose), MigrationResult (MigrationError, MigrationSuccess), Verbosity (Verbose), defaultOptions, runMigrations)
import Database.Postgres.Temp (CacheConfig (cacheDirectoryType), DirectoryType (Temporary), cacheAction, cacheConfig, defaultCacheConfig, toConnectionString, withDbCacheConfig)
import InMem.App (newApp)
import qualified InRel8.App as InRel8 (newApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Roundtrip (aesonRoundtripTests)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import StateMachine (checkInMemAppProp, checkRel8AppProp)
import Test.Tasty (localOption, testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Test.Tasty.QuickCheck (QuickCheckReplay (QuickCheckReplay), testProperties)

-- * Main

-- | @since 0.2.0.0
--
-- run tests
main :: IO ()
main = do
  specs <- aesonRoundtripTests
  mgr <- newManager defaultManagerSettings
  withDbCacheConfig defaultCacheConfig {cacheDirectoryType = Temporary} $ \(cacheConfig -> cfg) -> do
    let postgresDir = "./backend/postgres"
    migratedCfg <-
      cacheAction
        (postgresDir <> "/.tmp-postgres/cache")
        ( \(toConnectionString -> connStr) -> do
            let migrateDir = postgresDir <> "/migration"
                migrateTableCmd tbn = MigrationFile ("create " <> tbn <> " table") $ migrateDir <> "/" <> tbn <> ".sql"
                tables =
                  [ "accounts",
                    "articles",
                    "comments",
                    "tags",
                    "article_has_tag",
                    "user_follow_user",
                    "user_favorite_article"
                  ]
                cmds = migrateTableCmd <$> tables
            bracket
              (connectPostgreSQL connStr)
              close
              $ \conn ->
                runMigrations
                  conn
                  defaultOptions {optVerbose = Verbose}
                  ( MigrationInitialization :
                    cmds
                      <> [ MigrationValidation $
                             MigrationCommands cmds
                         ]
                  )
                  >>= \case
                    MigrationError err -> error $ fromString err
                    MigrationSuccess -> pure ()
        )
        cfg
        >>= either (error . show) pure
    let mkUrl port = BaseUrl Http "localhost" port ""
    defaultMainWithRerun $
      testGroup
        "tests"
        [ specs,
          localOption (QuickCheckReplay Nothing) $
            testProperties
              "state machine: "
              [ ("In mem app (seq)", checkInMemAppProp newApp mgr mkUrl),
                ("Rel8 app (seq)", checkRel8AppProp migratedCfg InRel8.newApp mgr mkUrl)
                -- NOTE: current state machine is not build for concurrency
                -- ("parallel", prop2 newApp mgr mkUrl)
              ]
        ]
