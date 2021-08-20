{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.QuestionAPI (questionServer, QuestionAPI) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.Persist
  ( Entity (..),
    insertEntity,
    selectList,
  )
import RIO
import Servant
import Server.API.Requests
import Server.Config (App (..))
import Server.Database.Model
import Server.Database.Queries (createQuestion, getAllQuestions, getQuestionById, updateQuestion)
import Server.Database.Setup (DbQuery, runDb)

type QuestionAPI =
  "api" :> "questions"
    :> ( Get '[JSON] [Entity Question]
           :<|> ReqBody '[JSON] CreateQuestionRequest
             :> Post '[JSON] (Entity Question)
           :<|> Capture "questionId" (Key Question)
             :> ReqBody '[JSON] UpdateQuestionRequest
             :> Patch '[JSON] (Entity Question)
       )

questionServer :: ServerT QuestionAPI App
questionServer =
  getQuestions
    :<|> postQuestion
    :<|> modifyQuestion

getQuestions :: App [Entity Question]
getQuestions = runDb getAllQuestions

postQuestion :: CreateQuestionRequest -> App (Entity Question)
postQuestion CreateQuestionRequest {..} = do
  now <- liftIO getCurrentTime
  runDb $ createQuestion (Question title content userId now now)

modifyQuestion :: Key Question -> UpdateQuestionRequest -> App (Entity Question)
modifyQuestion questionId UpdateQuestionRequest {..} = do
  runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  now <- liftIO getCurrentTime
  runDb $ updateQuestion questionId updatedContent now

