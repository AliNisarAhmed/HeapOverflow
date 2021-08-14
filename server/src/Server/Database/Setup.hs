{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Server.Database.Setup
  ( connectDb,
    runDb,
    migrateDb,
    DbQuery,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    asks,
  )
import Database.Persist
import Database.Persist.Postgresql
  ( ConnectionString,
    SqlBackend,
    createPostgresqlPool,
  )
import Database.Persist.Sql
  ( ConnectionPool,
    SqlPersistT,
    runMigration,
    runSqlPool,
  )
import Server.Config
import Server.Database.Model

connectDb :: ConnectionString -> IO ConnectionPool
connectDb connectionString =
  runStderrLoggingT $ createPostgresqlPool connectionString 1

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

migrateDb :: ConnectionPool -> IO ()
migrateDb pool = runSqlPool (runMigration migrateAll) pool

type DbQuery a = ReaderT SqlBackend IO a
