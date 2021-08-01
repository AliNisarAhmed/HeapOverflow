
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Server.Database.Setup
  ( connectDb
  , runDb
  , migrateDb
  )
where

import           Database.Persist
import           Database.Persist.Sql           ( SqlPersistT
                                                , runMigration
                                                , runSqlPool
                                                , ConnectionPool
                                                )

import           Database.Persist.Postgresql    ( ConnectionString
                                                , createPostgresqlPool
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                , asks
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( runStderrLoggingT )

import           Server.Config
import           Server.Database.Model

connectDb :: ConnectionString -> IO ConnectionPool
connectDb connectionString =
  runStderrLoggingT $ createPostgresqlPool connectionString 1

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

migrateDb :: ConnectionPool -> IO ()
migrateDb pool = runSqlPool (runMigration migrateAll) pool