module Server
  ( server,
    API,
  )
where

import Server.API.QuestionAPI
  ( QuestionApi,
    questionServer,
  )
import Server.Config (App (..))
import Data.Aeson
import Servant

type API = QuestionApi

server :: ServerT API App
server = questionServer