{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Server.API.QuestionAPI (questionServer, QuestionApi) where

import Server.Config (App (..))
import Server.Database.Setup ( runDb, runDb, DbQuery )
import Database.Persist
    ( Entity(..), selectList, Entity(..), selectList, insertEntity )
import Server.Database.Model
import Servant
import Server.Database.Queries (getAllQuestions, createQuestion)
import Server.API.Requests
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)

type QuestionApi =
    "api" :> "questions" :>
  (
    Get '[JSON] [Entity Question] :<|>
    ReqBody '[JSON] CreateQuestionRequest :> Post '[JSON] (Entity Question)
  )

questionServer :: ServerT QuestionApi App
questionServer = getQuestions :<|> postQuestion

getQuestions :: App [Entity Question]
getQuestions = runDb getAllQuestions

postQuestion :: CreateQuestionRequest -> App (Entity Question) 
postQuestion CreateQuestionRequest { title = t, content = c, userId = u } = do 
  now <- liftIO getCurrentTime 
  runDb $ createQuestion (Question t c u now now)
