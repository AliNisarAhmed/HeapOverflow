{-# LANGUAGE TypeOperators #-}

module Server
  ( server,
    API,
  )
where

import Data.Aeson
import RIO
import Servant
import qualified Servant.Auth.Server as SAS
import Server.API.AnswerAPI (AnswerAPI, answerServer)
import Server.API.AuthAPI (AuthAPI, AuthenticatedUser (AuthenticatedUser), authServer)
import Server.API.QuestionAPI
  ( QuestionAPI,
    questionServer,
  )
import Server.Config (App (..))

type API = AuthAPI :<|> QuestionAPI :<|> AnswerAPI

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API App
server cs jwts = authServer cs jwts :<|> questionServer :<|> answerServer
