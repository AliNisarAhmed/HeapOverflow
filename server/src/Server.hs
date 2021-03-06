{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Server
  ( appMain,
    api,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Database.Persist
import qualified Database.Persist.Migration as DPM (defaultSettings)
import Database.Persist.Migration.Postgres (runMigration)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import RIO hiding (Handler)
import Servant
import qualified Servant.Auth.Server as SAS
import Server.API.AnswerAPI (AnswerAPI, answerServer)
import Server.API.AuthAPI (AuthAPI, authServer)
import Server.API.QuestionAPI (QuestionAPI, questionServer)
import Server.Config
import Server.Database.Migrations (migration)
import Server.Database.Setup (connectDb, getDbConnString, migrateDb, runDb)
import Servant.Foreign
import GHC.TypeLits (KnownSymbol)

type API = QuestionAPI :<|> AnswerAPI :<|> AuthAPI

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API App
server cs jwts = questionServer :<|> answerServer :<|> authServer cs jwts 

api :: Proxy API
api = Proxy

-- Provide the missing HasForeign instance for AuthProtect, such that it is
-- compatible with JS generation and servant-options. See
--
-- * https://github.com/sordina/servant-options/issues/2
-- * https://github.com/haskell-servant/servant-auth/issues/8

-- instance (HasForeign lang ftype api) =>
--   HasForeign lang ftype (AuthProtect k :> api) where

--   type Foreign ftype (AuthProtect k :> api) = Foreign ftype api

--   foreignFor lang Proxy Proxy subR =
--     foreignFor lang Proxy (Proxy :: Proxy api) subR

-- instance (KnownSymbol sym, HasForeign lang ftype api)
--     => HasForeign lang ftype (AuthProtect sym :> api) where
--     type Foreign ftype (AuthProtect sym :> api) = Foreign ftype api

--     foreignFor lang ftype Proxy req =
--       foreignFor lang ftype (Proxy :: Proxy api) req

-- instance forall lang ftype api etc a.
--     ( HasForeign lang ftype api
--     , HasForeignType lang ftype T.Text
--     )
--   => HasForeign lang ftype (SAS.Auth (SAS.JWT ': etc) a :> api) where
--   type Foreign ftype (SAS.Auth (SAS.JWT ': etc) a :> api) = Foreign ftype api

--   foreignFor lang Proxy Proxy subR =
--     foreignFor lang Proxy (Proxy :: Proxy api) req
--     where
--       req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
--       arg = Arg
--         { _argName = PathSegment "Authorization"
--         , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy T.Text)
--         }

app ::
  Context '[SAS.CookieSettings, SAS.JWTSettings] ->
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  Config ->
  Application
app ctx cs jwts cfg =
    cors (const $ Just simpleCorsResourcePolicy) $ 
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
          corsRequestHeaders = ["Authorization", "Content-Type", "Options"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = False,
          corsRequireOrigin = False,
          corsIgnoreFailures = False
        }

convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg

appMain :: IO ()
appMain = do
  key <- SAS.generateKey
  putStrLn "starting the server"
  appEnv <- readAppEnv
  connStr <- getDbConnString
  pool <- connectDb (getNumberOfConn appEnv) connStr
  migrateDb pool
  port <- Server.Config.getPort
  let cfg = Config pool
      jwtCfg = SAS.defaultJWTSettings key
      cookieCfg = defineCookieConfig appEnv
      ctx = cookieCfg :. jwtCfg :. EmptyContext
  putStrLn $ "Server started on port " <> T.pack (show port)
  run port $ app ctx cookieCfg jwtCfg cfg
