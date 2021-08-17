{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Config where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
  )
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    createPostgresqlPool,
  )
import RIO hiding (Handler)
import Servant.Server
  ( Handler,
    ServerError,
  )

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a = AppT {runApp :: ReaderT Config Handler a}
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO, MonadThrow)

type App = AppT (IO ())

newtype Config = Config
  {configPool :: ConnectionPool}
  deriving (Show)
