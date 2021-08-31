{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.AnswerAPI where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental (Key)
import Database.Persist (Entity (Entity))
import RIO
import Servant
import Servant.API (ReqBody)
import qualified Servant.Auth.Server as SAS
import Server.API.AuthAPI (AuthenticatedUser (..))
import Server.API.Requests (CreateAnswerRequest (..), UpdateAnswerRequest (..), (!??))
import Server.Config (App)
import Server.Database.Model
import Server.Database.Queries (createAnswer, deleteAnswerById, getAnswerById, getAnswersByQuestionId, getQuestionById, updateAnswer)
import Server.Database.Setup (runDb)

type AnswerAPI =
  "api" :> "questions" :> Capture "questionId" (Key Question) :> "answers"
    :> ( Get '[JSON] [Entity Answer]
           :<|> ReqBody '[JSON] CreateAnswerRequest
             :> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser
             :> Post '[JSON] (Entity Answer)
           :<|> Capture "answerId" (Key Answer)
             :> ReqBody '[JSON] UpdateAnswerRequest
             :> Patch '[JSON] (Entity Answer)
           :<|> Capture "answerId" (Key Answer)
             :> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser
             :> Delete '[JSON] ()
       )

answerServer :: ServerT AnswerAPI App
answerServer questionId =
  getAnswersForQuestion questionId
    :<|> postAnswer questionId
    :<|> patchAnswer questionId
    :<|> deleteAnswer questionId

getAnswersForQuestion :: Key Question -> App [Entity Answer]
getAnswersForQuestion questionId =
  runDb $ getAnswersByQuestionId questionId

postAnswer :: Key Question -> CreateAnswerRequest -> SAS.AuthResult AuthenticatedUser -> App (Entity Answer)
postAnswer key CreateAnswerRequest {..} (SAS.Authenticated AuthenticatedUser {..})= do
  question <- runDb $ getQuestionById key
  now <- liftIO getCurrentTime
  case question of
    Nothing -> throwError $ err400 {errBody = "Question not found"}
    Just q -> runDb $ createAnswer key answerContent auId now
postAnswer _ _ _ = throwError err403

patchAnswer :: Key Question -> Key Answer -> UpdateAnswerRequest -> App (Entity Answer)
patchAnswer questionId answerId UpdateAnswerRequest {..} = do
  runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  runDb (getAnswerById answerId) !?? err400 {errBody = "Answer not found"}
  now <- liftIO getCurrentTime
  runDb $ updateAnswer answerId updatedContent now

deleteAnswer :: Key Question -> Key Answer -> SAS.AuthResult AuthenticatedUser -> App ()
deleteAnswer questionId answerId (SAS.Authenticated AuthenticatedUser {..}) = do
  runDb (getQuestionById questionId) !?? err400 {errBody = "Question not found"}
  answer <- runDb (getAnswerById answerId) !?? err400 {errBody = "Answer not found"}
  when (answerAuthorId answer /= auId) (throwError err403)
  void $ runDb (deleteAnswerById answerId)
deleteAnswer _ _ _ = throwError err403
