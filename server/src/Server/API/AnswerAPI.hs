{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.AnswerAPI where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental (Key)
import Database.Persist (Entity (Entity))
import Servant
import Servant.API (ReqBody)
import Server.API.Requests (CreateAnswerRequest (..), UpdateAnswerRequest (..), (!??))
import Server.Config (App)
import Server.Database.Model
import Server.Database.Queries (createAnswer, getAnswerById, getAnswersByQuestionId, getQuestionById, updateAnswer)
import Server.Database.Setup (runDb)

type AnswerAPI =
  "api" :> "questions" :> Capture "questionId" (Key Question) :> "answers"
    :> ( Get '[JSON] [Entity Answer]
           :<|> ReqBody '[JSON] CreateAnswerRequest :> Post '[JSON] (Entity Answer)
           :<|> Capture "answerId" (Key Answer) :> ReqBody '[JSON] UpdateAnswerRequest :> Patch '[JSON] (Entity Answer)
       )

answerServer :: ServerT AnswerAPI App
answerServer questionId =
  getAnswersForQuestion questionId
    :<|> postAnswer questionId
    :<|> patchAnswer questionId

getAnswersForQuestion :: Key Question -> App [Entity Answer]
getAnswersForQuestion questionId =
  runDb $ getAnswersByQuestionId questionId

postAnswer :: Key Question -> CreateAnswerRequest -> App (Entity Answer)
postAnswer key CreateAnswerRequest {..} = do
  question <- runDb $ getQuestionById key
  now <- liftIO getCurrentTime
  case question of
    Nothing -> throwError $ err400 {errBody = "Question not found"}
    Just q -> runDb $ createAnswer key answerContent authorId now

patchAnswer :: Key Question -> Key Answer -> UpdateAnswerRequest -> App (Entity Answer)
patchAnswer questionId answerId UpdateAnswerRequest {..} = do
  runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  runDb (getAnswerById answerId) !?? err400 {errBody = "Answer not found"}
  now <- liftIO getCurrentTime
  runDb $ updateAnswer answerId updatedContent now
