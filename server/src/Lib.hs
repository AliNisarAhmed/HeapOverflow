{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Database.Persist
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Server
  ( API,
    server,
  )
import Server.Config
import Server.Database.Setup (runDb)

app :: Config -> Application
app cfg =
  cors (const $ Just policy) $
    provideOptions api $
      serve
        api
        (appToServer cfg)
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Nothing,
          corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"],
          corsRequestHeaders = ["Authorization", "Content-Type"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = False,
          corsRequireOrigin = False,
          corsIgnoreFailures = False
        }

api :: Proxy API
api = Proxy

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server API
appToServer cfg = hoistServer api (convertApp cfg) server

convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg