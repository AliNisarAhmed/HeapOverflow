{-# LANGUAGE TypeOperators #-}

module Server
  ( server,
    API,
  )
where

import Server.API.QuestionAPI
  ( QuestionAPI,
    questionServer,
  )
import Server.Config (App (..))
import Data.Aeson
import Servant
import Server.API.AnswerAPI (answerServer, AnswerAPI)

type API = QuestionAPI :<|> AnswerAPI

server :: ServerT API App
server = questionServer :<|> answerServer
