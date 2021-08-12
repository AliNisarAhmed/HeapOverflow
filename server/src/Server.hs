{-# LANGUAGE TypeOperators #-}

module Server
  ( server,
    API,
  )
where

import Data.Aeson
import Servant
import Server.API.AnswerAPI (AnswerAPI, answerServer)
import Server.API.QuestionAPI
  ( QuestionAPI,
    questionServer,
  )
import Server.Config (App (..))

type API = QuestionAPI :<|> AnswerAPI

server :: ServerT API App
server = questionServer :<|> answerServer
