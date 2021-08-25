{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Server.Database.Setup
  ( connectDb,
    runDb,
    migrateDb,
    DbQuery,
    getDbConnString,
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
import RIO
import RIO.ByteString (ByteString)
import Server.Config
import Server.Core.Utils (fromEnv)
import Server.Database.Model
import System.Environment (getEnv, lookupEnv)
import Text.Read (read)

connectDb :: Int -> ConnectionString -> IO ConnectionPool
connectDb numPools connectionString =
  runStderrLoggingT $ createPostgresqlPool connectionString numPools

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

migrateDb :: ConnectionPool -> IO ()
migrateDb = runSqlPool (runMigration migrateAll)

type DbQuery a = ReaderT SqlBackend IO a

data DbConnection = DbConnection
  { dbHost :: ByteString,
    dbName :: ByteString,
    dbUser :: ByteString,
    dbPassword :: ByteString,
    dbPort :: ByteString
  }

devDbConnection :: DbConnection
devDbConnection =
  DbConnection
    { dbHost = "localhost",
      dbName = "oopsoverflow",
      dbUser = "postgres",
      dbPassword = "abc123",
      dbPort = "5432"
    }

getDbConnString :: IO ByteString
getDbConnString =
  fromEnv localConn "DATABASE_URL"
  where
    localConn = buildConnectionString devDbConnection

buildConnectionString :: DbConnection -> ByteString
buildConnectionString DbConnection {..} =
  "host=" <> dbHost
    <> " dbname="
    <> dbName
    <> " user="
    <> dbUser
    <> " password="
    <> dbPassword
    <> " port="
    <> dbPort
