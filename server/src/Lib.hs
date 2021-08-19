{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( appMain,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Database.Persist
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import RIO hiding (Handler)
import Servant
import qualified Servant.Auth.Server as SAS
import Server
  ( API,
    server,
  )
import Server.Config
import Server.Database.Setup (connectDb, migrateDb, runDb)

app ::
  Context '[SAS.CookieSettings, SAS.JWTSettings] ->
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  Config ->
  Application
app ctx cs jwts cfg =
  cors (const $ Just policy) $
    provideOptions api $
      serveWithContext api ctx $
        hoistServerWithContext
          api
          (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
          (convertApp cfg)
          (server cs jwts)
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

convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg

appMain :: IO ()
appMain = do
  key <- SAS.generateKey
  putStrLn "starting the server"
  let connectionString = "host=localhost dbname=oopsoverflow user=postgres password=abc123"
  pool <- connectDb connectionString
  migrateDb pool
  let cfg = Config pool
      port = 5000
      jwtCfg = SAS.defaultJWTSettings key
      cookieCfg = SAS.defaultCookieSettings
      ctx = cookieCfg :. jwtCfg :. EmptyContext
  putStrLn $ "Server started on port " <> T.pack (show port)
  run port $ app ctx cookieCfg jwtCfg cfg
