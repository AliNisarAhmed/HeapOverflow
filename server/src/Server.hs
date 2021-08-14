{-# LANGUAGE TypeOperators #-}

module Server
  ( server,
    API,
  )
where

import Data.Aeson
import Servant
import Server.API.AnswerAPI (AnswerAPI, answerServer)
import Server.API.AuthAPI (AuthAPI, authServer)
import Server.API.QuestionAPI
  ( QuestionAPI,
    questionServer,
  )
import Server.Config (App (..))

type API = AuthAPI :<|> QuestionAPI :<|> AnswerAPI

server :: ServerT API App
server = authServer :<|> questionServer :<|> answerServer
