{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module InRel8.StateMachine (inRel8AppStateMachinePropTest) where

import Control.Exception.Safe (bracket)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationCommands, MigrationFile, MigrationInitialization, MigrationValidation), MigrationResult (MigrationError, MigrationSuccess), defaultOptions, runMigrations)
import Database.Postgres.Temp (Cache, CacheConfig (cacheDirectoryType), Config, DB, DirectoryType (Temporary), cacheAction, cacheConfig, cleanupInitDbCache, defaultCacheConfig, setupInitDbCache, toConnectionString, withConfig)
import InRel8.App (newApp)
import Network.HTTP.Client (Manager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client (BaseUrl, mkClientEnv)
import StateMachine (stateMachine)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.Monadic (monadic)
import Test.StateMachine
  ( Reason (Ok),
    checkCommandNames,
    coverCommandNames,
    forAllCommands,
    prettyCommands,
    runCommands,
  )
import Test.Tasty (TestTree, withResource)
import Test.Tasty.QuickCheck (testProperty, (===))

postgresDir :: FilePath
postgresDir = "./backend/.postgres"

doMigration :: DB -> IO ()
doMigration config =
  let migrateTableCmd tbn = MigrationFile ("create " <> tbn <> " table") $ postgresDir <> "/migration/" <> tbn <> ".sql"
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
   in bracket
        (connectPostgreSQL $ toConnectionString config)
        close
        $ \conn ->
          runMigrations
            conn
            defaultOptions
            ( MigrationInitialization :
              cmds
                <> [ MigrationValidation $
                       MigrationCommands cmds
                   ]
            )
            >>= \case
              MigrationError err -> error $ fromString err
              MigrationSuccess -> pure ()

mkMigratedCacheConfig :: Cache -> IO Config
mkMigratedCacheConfig (cacheConfig -> cfg) =
  cacheAction (postgresDir <> "/.tmp-postgres/cache") doMigration cfg
    >>= either (error . show) pure

checkRel8AppProp :: IO Cache -> Manager -> (Int -> BaseUrl) -> Property
checkRel8AppProp getDbCache mgr mkUrl =
  forAllCommands stateMachine Nothing $ \cmds -> monadic
    ( ioProperty
        . \readerProp -> do
          migratedCfg <- getDbCache >>= mkMigratedCacheConfig
          withConfig
            migratedCfg
            ( \cfg ->
                testWithApplication (newApp $ toConnectionString cfg) $ \port ->
                  usingReaderT (mkClientEnv mgr $ mkUrl port) readerProp
            )
            >>= \case
              (Left se) -> error $ show se
              (Right prop') -> pure prop'
    )
    $ do
      (hist, _, res) <- runCommands stateMachine cmds
      prettyCommands stateMachine hist $ coverCommandNames cmds $ checkCommandNames cmds $ res === Ok

testWithPostgresResource :: (IO Cache -> TestTree) -> TestTree
testWithPostgresResource = withResource (setupInitDbCache defaultCacheConfig {cacheDirectoryType = Temporary}) cleanupInitDbCache

inRel8AppStateMachinePropTest :: Manager -> (Int -> BaseUrl) -> TestTree
inRel8AppStateMachinePropTest mgr mkUrl = testWithPostgresResource $ \getDbCache ->
  testProperty "Rel8 app (seq)" $ checkRel8AppProp getDbCache mgr mkUrl
