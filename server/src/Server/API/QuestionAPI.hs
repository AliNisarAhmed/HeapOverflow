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
import qualified Servant.Auth.Server as SAS
import Server.API.AuthAPI (AuthenticatedUser (..))
import Server.API.Requests
import Server.Config (App (..))
import Server.Database.Model
import Server.Database.Queries (createQuestion, deleteQuestionById, getAllQuestions, getQuestionById, updateQuestion, createTags, createQuestionTags)
import Server.Database.Setup (DbQuery, runDb)
import System.IO (print)
import Server.API.Responses (QuestionDTO, mkQuestionDTO)

type QuestionAPI =
  "api" :> "questions"
    :> ( Get '[JSON] [Entity Question]
           :<|> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser
             :> ReqBody '[JSON] CreateQuestionRequest
             :> Post '[JSON] QuestionDTO
           :<|> Capture "questionId" (Key Question)
             :> ReqBody '[JSON] UpdateQuestionRequest
             :> Patch '[JSON] (Entity Question)
           :<|> Capture "questionId" (Key Question)
             :> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser
             :> Delete '[JSON] ()
       )

questionServer :: ServerT QuestionAPI App
questionServer =
  getQuestions
    :<|> postQuestion
    :<|> modifyQuestion
    :<|> deleteQuestion

getQuestions :: App [Entity Question]
getQuestions = runDb getAllQuestions

postQuestion :: SAS.AuthResult AuthenticatedUser -> CreateQuestionRequest -> App QuestionDTO
postQuestion (SAS.Authenticated AuthenticatedUser {..}) CreateQuestionRequest {..} = do
  now <- liftIO getCurrentTime
  when (null tags) (throwError err400 {errBody = "At least one tag is required"} )
  createdQuestion <- runDb $ createQuestion (Question title content auId now now)
  tagIds <- runDb $ createTags tags
  runDb $ createQuestionTags tagIds (entityKey createdQuestion)
  pure $ mkQuestionDTO createdQuestion tags
postQuestion x _ = SAS.throwAll err401

modifyQuestion :: Key Question -> UpdateQuestionRequest -> App (Entity Question)
modifyQuestion questionId UpdateQuestionRequest {..} = do
  runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  now <- liftIO getCurrentTime
  runDb $ updateQuestion questionId updatedContent now

deleteQuestion :: Key Question -> SAS.AuthResult AuthenticatedUser -> App ()
deleteQuestion questionId (SAS.Authenticated AuthenticatedUser {..}) = do
  question <- runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  when (questionUserId question /= auId) (throwError err403)
  runDb (deleteQuestionById questionId)
  pure ()
deleteQuestion _ _ = throwError err403
