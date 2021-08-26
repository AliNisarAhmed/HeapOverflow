{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    createPostgresqlPool,
  )
import RIO hiding (Handler)
import Servant.Auth.Server (CookieSettings (..), IsSecure (..), defaultCookieSettings)
import Servant.Server
  ( Handler,
    ServerError,
  )
import Server.Core.Utils (fromEnv)

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

data Environment
  = Prod
  | Dev
  deriving (Eq, Show, Read, Generic)

instance ToJSON Environment

instance FromJSON Environment

newtype Config = Config
  { configPool :: ConnectionPool
  }
  deriving (Show)

getConfig :: IO Config
getConfig = undefined

readAppEnv :: IO Environment
readAppEnv = fromEnv Dev "APPLICATION_ENVIRONMENT"

getNumberOfConn :: Environment -> Int
getNumberOfConn Dev = 1
getNumberOfConn Prod = 8

defineCookieConfig :: Environment -> CookieSettings
defineCookieConfig env =
  case env of
    Dev -> defaultCookieSettings {cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing}
    Prod -> defaultCookieSettings
